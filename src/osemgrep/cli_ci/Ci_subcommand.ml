(* Yoann Padioleau, Robur
 *
 * Copyright (C) 2023-2025 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
open Fpath_.Operators
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-ci command, execute it and exit.

   Translated from ci.py (and partially from scans.py)

   See https://www.notion.so/semgrep/Architecture-Overview-CI-Scans-afe6193a6cc84abd96cff5f2d91cecaa
   for an excellent overview of how 'semgrep ci' communicate with the
   Semgrep App backend (the backend code is in semgrep-app/server/).
   See also https://www.notion.so/semgrep/Scan-reliability-next-steps-Oct-2023-cf3dad02d1ff4e1a98db8acf7f7bbded

   Debugging trick #1:
   --------------------

   If 'semgrep ci' returns some networking errors, you may need to inspect
   the backend logs as the error message returned by the backend to the CLI
   might be short and may not contain the necessary information to debug.
   Even using --debug might not be enough.

   You can use Sentry https://semgrep.sentry.io/issues/?statsPeriod=24h
   to look at the latest errors.

   As an example, here is a workflow that failed in the past:
   https://github.com/semgrep/semgrep/actions/runs/6599573075/job/17928762827
   Looking at the job log, we can see a problem when connecting to
   the https://semgrep.dev/api/agent/scans/14253285/complete endpoint.
   Then in Sentry you can paste this 'url: <URL>' in the query and search
   for errors related to this endpoint (you may need to replace the 'https'
   by 'http' sometimes to find something).

   Debugging trick #2:
   --------------------

   You can also inspect the backend logs in Datadog. If you know
   the scan_id of the problematic request, you can search for
   @scan_id:<id> at https://app.datadoghq.com/logs

   In the example above, the scan_id was 14253285.
   You will probably need also to setup the period in the upper right
   (e.g., select last few hours). If there are many matching logs,
   you can focus on the one with errors (usually tagged with a red
   rectangle on the left).

   Debugging trick #3:
   --------------------

   When using Datadog, it might be better first to connect to the 'dev2'
   backend rather than 'prod' to have a lot less logs to search through.
   You can filter out by `env: dev2` in Datadog. To connect to dev2,
   you'll need to run semgrep ci like this:

     SEMGREP_APP_URL=https://dev2.semgrep.dev SEMGREP_APP_TOKEN=... semgrep ci

   Note that you'll first need to

      SEMGREP_APP_URL=https://dev2.semgrep.dev semgrep login

   as you'll need a separate app token for dev2. You can find the
   actual token value in your ~/.semgrep/settings.yml file

   Tip: you can store those environment variables in a dev2.sh env file
   that you can source instead.

   Debugging trick #4:
   --------------------

   Use the --log-backend <dir> option to save all comms with the backend
   to a log directory and inspect the generated JSON files.
   See also --fake-backend <dir> option.

*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* This is mostly a superset of Scan_subcommand.caps so see the comment
 * in Scan_subcommand.ml for some explanations of why we need those
 * capabilities. Otherwise, here are the CI-specific explanations:
 * - Cap.exec for git
 * - Cap.tmp for decode_json_rules
 * TODO: remain fs related capabilities (mostly Cap.FS.files_argv_r)
 *)
type caps =
  < Cap.stdout
  ; Cap.network
  ; Cap.exec
  ; Cap.tmp
  ; Cap.chdir
  ; Cap.fork
  ; Cap.time_limit
  ; Cap.memory_limit >

(*****************************************************************************)
(* Log and test infra (logging/mocking the backend) *)
(*****************************************************************************)

(* This type will allow to mock our connections to the Semgrep WebApp backend
 * and make it easier to debug osemgrep ci.
 * This might also be needed at some point when porting test_ci.py
 *
 * alt: we could use Http_mock_client.ml to mock Semgrep_App.ml but
 *  it seems simpler to mock at the Semgrep_output_v1.atd "RPC" level the
 *  response rather than the HTTP GET/POST level.
 * alt: move in Semgrep_App.mli instead or in a Mock_Semgrep_App.mli
 *
 * Note that we need to have ['caps] polymorphic param below even though we
 * always use further below '<Cap.network; Auth.cap_token; ..>' because
 * we can't have '..' (a row type variable) inside a monomorphic record.
 *)
type 'caps app = {
  (* TODO: get rid of this, should not be needed anymore *)
  get_deployment_from_token : 'caps -> Out.deployment_config option;
  start_scan : 'caps -> Out.scan_request -> (Out.scan_response, string) result;
  upload_findings :
    'caps ->
    scan_id:int ->
    results:Out.ci_scan_results ->
    complete:Out.ci_scan_complete ->
    (Semgrep_App.app_block_override, string) result;
  report_failure : 'caps -> scan_id:int -> Exit_code.t -> unit;
}

let real_backend : < Cap.network ; Auth.cap_token ; .. > app =
  {
    get_deployment_from_token = Semgrep_App.get_deployment_from_token;
    start_scan = Semgrep_App.start_scan;
    upload_findings = Semgrep_App.upload_findings;
    report_failure = Semgrep_App.report_failure;
  }

(* This is for 'osemgrep ci --log-backend /tmp/log_dir/' to save all comms
 * with the backend for debugging purpose. The generated data can also
 * be useful for --fake-backend as a first draft (see further below).
 *
 * alt: do that in Semgrep_App.ml
 * alt: use Logs.debug instead of saving in files but those JSON can be really
 *  bit so better not use Logs.debug(). We spent lots of time making the --debug
 *  logs reasonable in size (that is fitting in GHA log window) so let's keep it
 *  that way.
 *  Moreover it is easier with those separate JSON files to go directly to the
 *  relevant logged JSON response from the backend.
 *)

let mk_log_backend (dir : Fpath.t) : < Cap.network ; Auth.cap_token ; .. > app =
  if not (UFile.is_dir ~follow_symlinks:false dir) then
    UFile.make_directories dir;
  {
    get_deployment_from_token = real_backend.get_deployment_from_token;
    start_scan =
      (fun caps request ->
        let file = dir / "scan_request.json" in
        Logs.debug (fun m -> m "saving scan_request in %s" !!file);
        let str = Out.string_of_scan_request request |> JSON.prettify in
        UFile.write_file ~file str;
        let res = real_backend.start_scan caps request in
        (match res with
        | Error _ -> ()
        | Ok scan_response ->
            let file = dir / "scan_response.json" in
            Logs.debug (fun m -> m "saving start_scan response in %s" !!file);
            (* let's not prettify here as the string can be 50MB sometimes *)
            let str = Out.string_of_scan_response scan_response in
            UFile.write_file ~file str);
        res);
    upload_findings =
      (fun caps ~scan_id ~results ~complete ->
        let file = dir / "results.json" in
        Logs.debug (fun m -> m "saving results in %s" !!file);
        let str = Out.string_of_ci_scan_results results |> JSON.prettify in
        UFile.write_file ~file str;

        let file = dir / "complete.json" in
        Logs.debug (fun m -> m "saving complete in %s" !!file);
        let str = Out.string_of_ci_scan_complete complete |> JSON.prettify in
        UFile.write_file ~file str;

        (* less: save the response from the backend *)
        real_backend.upload_findings caps ~scan_id ~results ~complete);
    report_failure = real_backend.report_failure;
  }

(* This is for 'osemgrep ci --fake-backend tests/ci/fake_backend/' to
 * fake all comms with the backend for debugging (and possibly testing) purpose.
 *
 * alt: we could create a separate toy repo and setup the CI config for it
 *  via the WebApp so one could debug osemgrep ci more easily than on the
 *  semgrep repo which downloads 50MB of rules each time. However, using
 *  the --fake-backend option is faster as one can modify directly
 *  some /fake_backend/.../scan_response.json and we can use those
 *  fake CI config on the semgrep repo itself.
 *)
let mk_fake_backend base (dir : Fpath.t) :
    < Cap.network ; Auth.cap_token ; .. > app =
  {
    get_deployment_from_token = base.get_deployment_from_token;
    start_scan =
      (fun _caps _request ->
        let file = dir / "scan_response.json" in
        let str = UFile.read_file file in
        let resp = Out.scan_response_of_string str in
        Logs.debug (fun m -> m "faking start_scan response from %s" !!file);

        (* This gives the ability to override what is in scan_response.json
         * rules field with the content of a separate file so one can more
         * easily adjust the rules.
         *)
        let rule_file = dir / "rules.json" in
        let resp =
          if Sys.file_exists !!rule_file then begin
            Logs.debug (fun m ->
                m "overriding rules in %s using %s" !!file !!rule_file);
            let rules = UFile.read_file rule_file |> Yojson.Basic.from_string in
            { resp with config = { resp.config with rules } }
          end
          else resp
        in
        Ok resp);
    (* Note that you will most likely get an error from the backend
     * when uploading the findings because the scan_id in the
     * scan_response.json has already been used for a previous scan.
     *
     * LATER: we could also mock this endpoint and possibly intercept and save
     * in some JSON files for snapshot testing like in test_ci.py
     *)
    upload_findings = base.upload_findings;
    report_failure = base.report_failure;
  }

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

let exit_code_of_blocking_findings ~audit_mode ~on ~app_block_override
    blocking_findings : Exit_code.t =
  match (blocking_findings, app_block_override, audit_mode) with
  | _, Some reason, false ->
      Logs.app (fun m ->
          m "  semgrep.dev is suggesting a non-zero exit code (%s)" reason);
      Exit_code.findings ~__LOC__
  | _ :: _, _, true ->
      Logs.app (fun m ->
          m
            "  Audit mode is on for %s, so exiting with code 0 even if matches \
             found"
            on);
      Exit_code.ok ~__LOC__
  | _ :: _, _, false ->
      Logs.app (fun m ->
          m "  Has findings for blocking rules so exiting with code 1");
      Exit_code.findings ~__LOC__
  | [], _, _ ->
      Logs.app (fun m -> m "  No blocking findings so exiting with code 0");
      Exit_code.ok ~__LOC__

let sanity_check_contributions (contribs : Out.contribution list) : unit =
  match contribs with
  | [] -> Logs.warn (fun m -> m "no contribution found from git log, weird")
  | x :: _xs ->
      Logs.debug (fun m ->
          m "found %d contribs in the last 30 days, last is %s"
            (List.length contribs)
            (Out.string_of_contribution x))

(*****************************************************************************)
(* Project metadata *)
(*****************************************************************************)

(* from meta.py
 * coupling: if you add more cases below, you probably need to modify
 * Ci_CLI.cmdline_term to pass more env there.
 *)
let generate_meta_from_environment caps (baseline_ref : Digestif.SHA1.t option)
    : Project_metadata.t =
  let extract_env term =
    let argv = [| "empty" |] and info_ = Cmdliner.Cmd.info "" in
    let eval term =
      match Cmdliner.Cmd.(eval_value ~argv (v info_ term)) with
      | Ok (`Ok env) -> env
      | Ok `Version
      | Ok `Help ->
          invalid_arg "unexpected version or help"
      | Error _e -> invalid_arg "couldn't decode environment"
    in
    eval term
  in

  match Sys.getenv_opt "GITHUB_ACTIONS" with
  | Some "true" ->
      let env = extract_env Git_metadata.env in
      let gha_env = extract_env Github_metadata.env in
      (new Github_metadata.meta caps baseline_ref env gha_env)#project_metadata
  | _else ->
      let env = extract_env Git_metadata.env in
      (new Git_metadata.meta caps ~scan_environment:"git" ~baseline_ref env)
        #project_metadata

(* https://docs.gitlab.com/ee/ci/variables/predefined_variables.html *)
(* match Sys.getenv_opt "GITLAB_CI" with
   | Some "true" -> return GitlabMeta(baseline_ref)
   | _else -> *)
(* https://circleci.com/docs/2.0/env-vars/#built-in-environment-variables *)
(* match Sys.getenv_opt "CIRCLECI" with
   | Some "true" -> return CircleCIMeta(baseline_ref)
   | _else -> *)
(* https://e.printstacktrace.blog/jenkins-pipeline-environment-variables-the-definitive-guide/ *)
(* match Sys.getenv_opt "JENKINS_URL" with
    | Some _ -> return JenkinsMeta(baseline_ref)
    | None -> *)
(* https://support.atlassian.com/bitbucket-cloud/docs/variables-and-secrets/ *)
(* match Sys.getenv_opt "BITBUCKET_BUILD_NUMBER" with
   | Some _ -> return BitbucketMeta(baseline_ref)
   | None -> *)
(* https://github.com/DataDog/dd-trace-py/blob/f583fec63c4392a0784b4199b0e20931f9aae9b5/ddtrace/ext/ci.py#L90
   picked an env var that is only defined by Azure Pipelines *)
(* match Sys.getenv_opt "BUILD_BUILDID" with
   | Some _ -> AzurePipelinesMeta(baseline_ref)
   | None -> *)
(* https://buildkite.com/docs/pipelines/environment-variables#bk-env-vars-buildkite-build-author-email *)
(* match Sys.getenv_opt "BUILDKITE" with
   | Some "true" -> return BuildkiteMeta(baseline_ref)
   | _else -> *)
(* https://docs.travis-ci.com/user/environment-variables/ *)
(* match Sys.getenv_opt "TRAVIS" with
   | Some "true" -> return TravisMeta(baseline_ref)
   | _else -> return GitMeta(baseline_ref) *)

(*****************************************************************************)
(* Scan metadata *)
(*****************************************************************************)

let scan_metadata () : Out.scan_metadata =
  let res =
    Out.
      {
        cli_version = Version.version;
        unique_id = Uuidm.v4_gen (Stdlib.Random.State.make_self_init ()) ();
        (* TODO: should look at conf.secrets, conf.sca, conf.code, etc. *)
        requested_products = [];
        dry_run = false;
        sms_scan_id = !Semgrep_envvars.v.sms_scan_id;
      }
  in
  res.sms_scan_id
  |> Option.iter (fun scan_id ->
         Logs.debug (fun m -> m "SMS scan id: %s" scan_id));
  res

(*****************************************************************************)
(* Project config *)
(*****************************************************************************)
(* TODO: read the .semgrepconfig.yml in the repo *)
let project_config () : Out.ci_config_from_repo option =
  (* alt: Out.{ version = "v1"; tags = None; } *)
  None

(*****************************************************************************)
(* Scan config *)
(*****************************************************************************)
(* token -> deployment_config -> scan_id -> scan_config -> rules *)

let caps_with_token (token_opt : Auth.token option) caps =
  let token =
    match token_opt with
    | Some tok -> tok
    | None ->
        Logs.app (fun m ->
            m
              "run `semgrep login` before using `semgrep ci` or use `semgrep \
               scan` and set `--config`");
        Error.exit_code_exn (Exit_code.invalid_api_key ~__LOC__)
  in
  Auth.cap_token_and_network_and_tmp_and_exec token caps

(* if something fails, we Error.exit_code_exn *)
let deployment_config (caps : < Cap.network ; Auth.cap_token ; .. >) app :
    Out.deployment_config =
  match app.get_deployment_from_token caps with
  | None ->
      Logs.app (fun m ->
          m
            "API token not valid. Try to run `semgrep logout` and `semgrep \
             login` again. Or in CI, ensure your SEMGREP_APP_TOKEN variable is \
             set correctly.");
      Error.exit_code_exn (Exit_code.invalid_api_key ~__LOC__)
  | Some deployment_config ->
      Logs.debug (fun m ->
          m "received deployment = %s"
            (Out.show_deployment_config deployment_config));
      deployment_config

(* [rules] contains the rules in JSON format. That's how the registry send
 * them because it's faster than using YAML.
 * TODO: factorize with Session.decode_rules()
 *)
let decode_json_rules caps (rules : Yojson.Basic.t) :
    Rule_fetching.rules_and_origin =
  (* TODO: ugly to have to convert back to string and use intermediate tmp
   * file. We should optimize and parse directly from the JSON data structure
   *)
  let data = Yojson.Basic.to_string rules in
  CapTmp.with_temp_file caps#tmp ~contents:data ~suffix:".json" (fun file ->
      match
        Rule_fetching.load_rules_from_file ~rewrite_rule_ids:false ~origin:App
          caps file
      with
      | Ok rules -> rules
      | Error _err ->
          (* There shouldn't be any errors, because we obtained these rules
             from CI.
          *)
          failwith "impossible: received an invalid rule from CI")

let scan_config_and_rules_from_deployment
    (caps : < Cap.network ; Auth.cap_token ; .. >) app
    (prj_meta : Out.project_metadata)
    (deployment_config : Out.deployment_config) :
    Semgrep_App.scan_id
    * Out.scan_response
    * Rule_fetching.rules_and_origin list =
  Logs.app (fun m -> m "  %s" (Console.underline "CONNECTION"));
  (* TODO: pysemgrep: console spinner with Initializing scan ... instead *)
  Logs.app (fun m ->
      m "  Requesting scan (deployment = %s)" deployment_config.name);
  let scan_metadata : Out.scan_metadata = scan_metadata () in
  let project_config : Out.ci_config_from_repo option = project_config () in

  let request : Out.scan_request =
    { project_metadata = prj_meta; scan_metadata; project_config }
  in
  (* TODO:
      metadata_dict["is_sca_scan"] = supply_chain
      proj_config = ProjectConfig.load_all()
  *)
  match app.start_scan caps request with
  | Error msg ->
      Logs.err (fun m -> m "Could not start scan %s" msg);
      Error.exit_code_exn (Exit_code.fatal ~__LOC__)
  | Ok scan_response ->
      let scan_id =
        match scan_response.info.id with
        | Some id -> id
        | None -> failwith "TODO: handle dry-run"
      in
      (* TODO: pysemgrep: should replace the Requesting scan of before *)
      Logs.app (fun m ->
          m "  Initializing scan (deployment=%s, scan_id=%d)"
            deployment_config.name scan_id);

      let rules_and_origins =
        try
          decode_json_rules
            (caps :> < Cap.network ; Cap.tmp >)
            scan_response.config.rules
        with
        | Error.Semgrep_error (_, opt_ex) as e ->
            let ex =
              match opt_ex with
              | None -> Exit_code.fatal ~__LOC__
              | Some exit_code -> exit_code
            in
            app.report_failure caps ~scan_id ex;
            let e = Exception.catch e in
            Exception.reraise e
      in
      (scan_id, scan_response, [ rules_and_origins ])

(*****************************************************************************)
(* Partition rules *)
(*****************************************************************************)
let finding_is_blocking (m : Out.cli_match) =
  let contains_blocking xs =
    List.exists
      (function
        | JSON.String s -> String.equal s "block"
        | _ -> false)
      xs
  in

  let validation_state_to_action (vs : Out.validation_state) =
    match vs with
    | `Confirmed_valid -> "valid"
    | `Confirmed_invalid -> "invalid"
    | `Validation_error -> "error"
    | `No_validator -> "valid" (* Fallback to valid action for no validator *)
  in

  let metadata = JSON.from_yojson m.extra.metadata in

  match metadata with
  | JSON.Object xs -> (
      match
        ( m.extra.validation_state,
          List.assoc_opt "dev.semgrep.validation_state.actions" xs,
          List.assoc_opt "dev.semgrep.actions" xs )
      with
      | Some validation_state, Some (JSON.Object vs), _ ->
          List.assoc_opt (validation_state_to_action validation_state) vs
          |> Option.map (JSON.equal (JSON.String "block"))
          |> Option.value ~default:false
      | None, _, Some (JSON.Array actions) -> contains_blocking actions
      | _ -> false)
  | _ -> false

let rule_is_blocking (json : JSON.t) =
  match json with
  | JSON.Object xs -> (
      match List.assoc_opt "dev.semgrep.validation_state.actions" xs with
      | Some (JSON.Object vs) ->
          List.exists
            (function
              | _, JSON.String s -> String.equal s "block"
              | _ -> false)
            vs
      | _ -> (
          match List.assoc_opt "dev.semgrep.actions" xs with
          | Some (JSON.Array stuff) ->
              List.exists
                (function
                  | JSON.String s -> String.equal s "block"
                  | _ -> false)
                stuff
          | _ -> false))
  | _ -> false

(* partition rules *)
let partition_rules (filtered_rules : Rule.t list) =
  let cai_rules, rest =
    filtered_rules
    |> List.partition (fun r ->
           Common2.string_match_substring
             (Str.regexp "r2c-internal-cai")
             (Rule_ID.to_string (fst r.Rule.id)))
  in
  let blocking_rules, non_blocking_rules =
    rest
    |> List.partition (fun r ->
           Option.value ~default:false
             (Option.map rule_is_blocking r.Rule.metadata))
  in
  (cai_rules, blocking_rules, non_blocking_rules)

let partition_findings ~keep_ignored (results : Out.cli_match list) =
  let groups =
    results
    |> List.filter (fun (m : Out.cli_match) ->
           Option.value ~default:false m.extra.is_ignored && not keep_ignored)
    |> Assoc.group_by (fun (m : Out.cli_match) ->
           if
             Common2.string_match_substring
               (Str.regexp "r2c-internal-cai")
               (Rule_ID.to_string m.check_id)
           then `Cai
           else if finding_is_blocking m then
             (* and "sca_info" not in match.extra *)
             `Blocking
           else `Non_blocking)
  in
  ( (try List.assoc `Cai groups with
    | Not_found -> []),
    (try List.assoc `Blocking groups with
    | Not_found -> []),
    try List.assoc `Non_blocking groups with
    | Not_found -> [] )

(*****************************************************************************)
(* Conversions *)
(*****************************************************************************)

(* from rule_match.py
 * Note that the backend is moving to pull severity from the rules rather
 * than the findings so in practice the value we put here might be
 * ignored in the futur.
 *)
let severity_to_int (severity : Rule.severity) =
  match severity with
  | `Inventory
  | `Info
  | `Low ->
      `Int 0
  | `Warning
  | `Medium ->
      `Int 1
  | `Error
  | `High ->
      `Int 2
  | `Critical -> `Int 3
  | `Experiment -> `Int 4

(* this is used for sorting matches for findings *)
let ord_of_severity (severity : Rule.severity) : int =
  match severity with
  | `Experiment -> 0
  | `Inventory -> 1
  | `Info
  | `Low ->
      2
  | `Warning
  | `Medium ->
      3
  | `Error
  | `High ->
      4
  | `Critical -> 5

let finding_of_cli_match _commit_date index (m : Out.cli_match) : Out.finding =
  let (r : Out.finding) =
    {
      check_id = m.check_id;
      path = m.path;
      line = m.start.line;
      column = m.start.col;
      end_line = m.end_.line;
      end_column = m.end_.col;
      message = m.extra.message;
      severity = severity_to_int m.extra.severity;
      index;
      engine_kind = m.extra.engine_kind;
      commit_date = "";
      (* TODO datetime.fromtimestamp(int(commit_date)).isoformat() *)
      syntactic_id = "";
      (* TODO, see rule_match.py *)
      match_based_id = None;
      (* TODO: see rule_match.py *)
      hashes = None;
      (* TODO should compute start_line_hash / end_line_hash / code_hash / pattern_hash *)
      metadata = m.extra.metadata;
      is_blocking = finding_is_blocking m;
      fixed_lines =
        None
        (* TODO: if self.extra.get("fixed_lines"): ret.fixed_lines = self.extra.get("fixed_lines") *);
      sca_info = None;
      (* TODO *)
      dataflow_trace = None;
      validation_state = None;
      historical_info = None;
    }
  in
  r

(*****************************************************************************)
(* Reporting *)
(*****************************************************************************)

let report_scan_environment (prj_meta : Out.project_metadata) : unit =
  Logs.app (fun m -> m "  %s" (Console.underline "SCAN ENVIRONMENT"));
  Logs.app (fun m ->
      m "  versions    - semgrep %s on OCaml %s"
        (Console.bold Version.version)
        (Console.bold Sys.ocaml_version));
  Logs.app (fun m ->
      m "  environment - running in environment %s, triggering event is %s\n"
        (Console.bold prj_meta.scan_environment)
        (Console.bold prj_meta.on));
  ()

let report_scan_completed ~blocking_findings ~blocking_rules
    ~non_blocking_findings ~non_blocking_rules =
  Logs.app (fun m -> m "CI scan completed successfully.");
  Logs.app (fun m ->
      m "  Found %s (%u blocking) from %s."
        (String_.unit_str
           (List.length blocking_findings + List.length non_blocking_findings)
           "finding")
        (List.length blocking_findings)
        (String_.unit_str
           (List.length blocking_rules + List.length non_blocking_rules)
           "rule"));
  ()

(*****************************************************************************)
(* Uploading findings *)
(*****************************************************************************)

(* from scans.py *)
let findings_and_complete ~has_blocking_findings ~commit_date ~engine_requested
    (caps : < Cap.exec >) (cli_output : Out.cli_output) (rules : Rule.rule list)
    : Out.ci_scan_results * Out.ci_scan_complete =
  let targets = cli_output.paths.scanned in
  let skipped = cli_output.paths.skipped in

  let rule_ids = rules |> List_.map (fun r -> fst r.Rule.id) in
  let contributions = Parse_contribution.get_contributions caps in
  sanity_check_contributions contributions;

  (*
      we want date stamps assigned by the app to be assigned such that the
      current sort by relevant_since results in findings within a given scan
      appear in an intuitive order.  this requires reversed ordering here.
     *)
  let all_matches = List.rev cli_output.results in
  let all_matches =
    let sort_severity a b =
      Int.compare (ord_of_severity a) (ord_of_severity b)
    in
    all_matches
    |> List.sort (fun (m1 : Out.cli_match) (m2 : Out.cli_match) ->
           sort_severity m1.extra.severity m2.extra.severity)
  in
  let new_ignored, new_matches =
    all_matches
    |> List.partition (fun (m : Out.cli_match) ->
           Option.value ~default:false m.extra.is_ignored)
  in
  let findings = List_.mapi (finding_of_cli_match commit_date) new_matches in
  let ignores = List_.mapi (finding_of_cli_match commit_date) new_ignored in
  let ci_token =
    match Sys.getenv_opt "GITHUB_TOKEN" with
    (* GitHub (cloud) *)
    | Some _ as t -> t
    | None -> (
        match Sys.getenv_opt "GITLAB_TOKEN" with
        (* GitLab.com (cloud) *)
        | Some _ as t -> t
        | None -> Sys.getenv_opt "BITBUCKET_TOKEN" (* Bitbucket Cloud *))
  in
  (* POST to /api/agent/scans/<scan_id>/results *)
  let results : Out.ci_scan_results =
    {
      (* send a backup token in case the app is not available *)
      token = ci_token;
      findings;
      ignores;
      searched_paths = List.sort Fpath.compare targets;
      (* TODO: get renamed_paths, depends on baseline_commit *)
      renamed_paths = [];
      rule_ids;
      contributions = Some contributions;
      (* TODO: Figure out correct value for this. *)
      dependencies = None;
    }
  in
  if
    new_ignored
    |> List.exists (fun (m : Out.cli_match) -> m.extra.severity =*= `Experiment)
  then
    Logs.app (fun m -> m "Some experimental rules were run during execution.");

  let ignored_ext_freqs =
    Option.value ~default:[] skipped
    |> Assoc.group_by (fun (skipped_target : Out.skipped_target) ->
           Fpath.get_ext skipped_target.path)
    |> List.filter (fun (ext, _) -> not (String.equal ext ""))
    (* don't count files with no extension *)
    |> List_.map (fun (ext, xs) -> (ext, List.length xs))
  in

  (* POST to /api/agent/scans/<scan_id>/complete *)
  let complete : Out.ci_scan_complete =
    {
      (* TODO: 'and not match.is_ignored for match in all_matches' *)
      exit_code = (if has_blocking_findings then 1 else 0);
      (* TODO [e.to_json() for e in dependency_parser_errors], *)
      dependency_parser_errors = Some [];
      stats =
        {
          (* TODO: 'if not match.from_transient_scan' *)
          findings = List.length new_matches;
          errors = cli_output.errors;
          (* TODO: *)
          total_time = 0.0;
          unsupported_exts = ignored_ext_freqs;
          (* TODO dependency_counts =
           * {k:len(v) for k,v in lockfile_dependencies.items()} *)
          lockfile_scan_info = [];
          (* TODO: lang: {
              "targets_parsed": data.num_targets - data.targets_with_errors,
              "num_targets": data.num_targets,
              "bytes_parsed": data.num_bytes - data.error_bytes,
              "num_bytes": data.num_bytes,
              }
             for (lang, data) in parse_rate.get_errors_by_lang().items()
          *)
          parse_rate = [];
          engine_requested =
            Some (Semgrep_output_v1_j.string_of_engine_kind engine_requested);
          (* TODO: findings_by_product *)
          findings_by_product = None;
          (* TODO: supply_chain_stats *)
          supply_chain_stats = None;
        };
      (* TODO:
           if self._dependency_query:
               lockfile_dependencies_json = {}
               for path, dependencies in lockfile_dependencies.items():
                   lockfile_dependencies_json[path] = [
                       dependency.to_json() for dependency in dependencies
                   ]
               complete["dependencies"] = lockfile_dependencies_json
      *)
      dependencies = Some [];
      (* ??? *)
      task_id = None;
      final_attempt = None;
    }
  in
  (results, complete)

let upload_findings (caps : < Cap.network ; Auth.cap_token ; Cap.exec ; .. >)
    app (deployment_config : Out.deployment_config)
    (scan_id : Semgrep_App.scan_id) (prj_meta : Out.project_metadata)
    blocking_findings filtered_rules (cli_output : Out.cli_output) :
    Semgrep_App.app_block_override =
  Logs.app (fun m -> m "  Uploading findings.");
  let results, complete =
    findings_and_complete
      (caps :> < Cap.exec >)
      ~has_blocking_findings:(not (List_.null blocking_findings))
      ~commit_date:"" ~engine_requested:`OSS cli_output filtered_rules
  in
  let override =
    match app.upload_findings caps ~scan_id ~results ~complete with
    | Ok a -> a
    | Error msg ->
        Logs.err (fun m -> m "Failed to report findings: %s" msg);
        None
  in
  let repo_display_name =
    (* It should be impossible for repo_display_name to be None, but for
       backwards compatability the Out type is an optional *)
    Option.value ~default:"<YOUR_REPO_NAME>" prj_meta.repo_display_name
  in
  let ref_if_branch_detected =
    Option.fold ~none:"" ~some:(fun branch -> "&ref=" ^ branch) prj_meta.branch
  in
  Logs.app (fun m -> m "  View results in Semgrep Cloud Platform:");
  Logs.app (fun m ->
      m "    %s/orgs/%s/findings?repo=%s%s"
        (Uri.to_string !Semgrep_envvars.v.semgrep_url)
        deployment_config.name repo_display_name ref_if_branch_detected);
  if
    filtered_rules
    |> List.exists (fun r ->
           String.equal "r2c-internal-project-depends-on"
             (Rule_ID.to_string (fst r.Rule.id)))
  then
    Logs.app (fun m ->
        m "    %s/orgs/%s/supply-chain"
          (Uri.to_string !Semgrep_envvars.v.semgrep_url)
          deployment_config.name);
  override

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run_conf (caps : < caps ; .. >) (ci_conf : Ci_CLI.conf) : Exit_code.t =
  let conf = ci_conf.scan_conf in
  (match conf.common.maturity with
  (* coupling: copy-pasted from Scan_subcommand.ml *)
  | Maturity.Default -> (
      (* TODO: handle more confs, or fallback to pysemgrep further down *)
      match conf with
      (* for now we allways fallback to pysemgrep :( *)
      | _else_ -> raise Pysemgrep.Fallback)
  | Maturity.Legacy -> raise Pysemgrep.Fallback
  | Maturity.Experimental
  | Maturity.Develop ->
      ());

  (* test infra. see tests/ci/fake_backend/ for more info *)
  let app : _ app =
    match (ci_conf.fake_backend, ci_conf.log_backend) with
    | None, None -> real_backend
    | None, Some dir -> mk_log_backend dir
    | Some dir, None -> mk_fake_backend real_backend dir
    | Some dir_fake, Some dir_log ->
        mk_fake_backend (mk_log_backend dir_log) dir_fake
  in

  (* step1: initialization *)
  CLI_common.setup_logging ~force_color:conf.output_conf.force_color
    ~level:conf.common.logging_level;
  Logs.debug (fun m -> m "conf = %s" (Ci_CLI.show_conf ci_conf));

  (* TODO? we probably want to set the metrics to On by default in CI ctx? *)
  Metrics_.configure conf.metrics;
  let settings = Semgrep_settings.load ~maturity:conf.common.maturity () in
  let _dry_runTODO = conf.output_conf.fixed_lines in

  (* step2: sanity checking *)
  (match conf.rules_source with
  | Configs [] -> ()
  | _else_ ->
      Logs.app (fun m ->
          m
            "Cannot run `semgrep ci` with --config. The `semgrep ci` command \
             will upload findings to semgrep-app and those findings must come \
             from rules configured there. Drop the `--config` to use rules \
             configured on semgrep.dev or use semgrep scan.");
      Error.exit_code_exn (Exit_code.fatal ~__LOC__));
  (* see Ci_CLI.scan_subset_cmdline_term() *)
  assert (conf.target_roots =*= []);

  (* step3: token -> deployment_config -> scan_id -> scan_config -> rules *)
  let caps' = caps_with_token settings.api_token caps in
  let depl = deployment_config caps' app in
  (* TODO: pass baseline commit! *)
  let prj_meta = generate_meta_from_environment (caps :> < Cap.exec >) None in
  Logs.app (fun m -> m "%s" (Console.heading "Debugging Info"));
  report_scan_environment prj_meta;

  (* After sanity checking, we either
   * (1) reach out to the server to get the config and then do a scan
   * (2) perform one of the distributed scan steps and exit
   *)

  (* ===== Begin of steps related to distributed scans ===== *)
  (* If we are doing a distributed scan step, complete the step, then exit *)
  Distributed_scan_stub.maybe_merge_partial_scan_results_then_exit
    ci_conf.x_distributed_scan_conf;
  Distributed_scan_stub.maybe_validate_partial_scan_results_then_exit
    ci_conf.x_distributed_scan_conf;

  (* ===== End of steps related to distributed scans ===== *)

  (* TODO: fix_head_if_github_action(metadata) *)
  let scan_id, scan_response, rules_and_origin =
    scan_config_and_rules_from_deployment caps' app prj_meta depl
  in

  (* TODO: we should use those fields!
   * TODO: add 'actions' to scan_response and 'ci_config_from_cloud'
   *)
  let {
    info =
      {
        (* used in scan_config_and_rules_from_deployment *)
        id = _;
        (* TODO: use! *)
        enabled_products = _;
        (* TODO? redundant with info from deployment ? *)
        deployment_id = _;
        deployment_name = _;
      };
    config =
      {
        (* used in scan_config_and_rules_from_deployment *)
        rules = _;
        (* TODO: use ? *)
        triage_ignored_syntactic_ids = _;
        triage_ignored_match_based_ids = _;
      };
    (* TODO: lots of things to use there *)
    engine_params =
      {
        autofix = _;
        deepsemgrep = _;
        dependency_query = _;
        path_to_transitivity = _;
        scan_all_deps_in_diff_scan = _;
        ignored_files = _;
        product_ignored_files = _;
        generic_slow_rollout = _;
        historical_config = _;
        always_suppress_errors = _;
      };
  } : Out.scan_response =
    scan_response
  in

  (* TODO:
     if dataflow_traces is None:
       dataflow_traces = engine_type.has_dataflow_traces
     if max_memory is None:
       max_memory = engine_type.default_max_memory
     if interfile_timeout is None:
       interfile_timeout = engine_type.default_interfile_timeout
     if engine_type.is_pro:
       console.print(Padding(Title("Engine", order=2), (1, 0, 0, 0)))
       if engine_type.check_if_installed():
         console.print(
           f"Using Semgrep Pro Version: [bold]{engine_type.get_pro_version()}[/bold]",
            markup=True,
         )
         console.print(
           f"Installed at [bold]{engine_type.get_binary_path()}[/bold]",
           markup=True,
         )
     else:
       run_install_semgrep_pro()
  *)
  (* TODO
     excludes_from_app = scan_handler.ignore_patterns if scan_handler else []
     assert exclude is not None  # exclude is default empty tuple
     exclude = ( *exclude, *yield_exclude_paths(excludes_from_app))
  *)

  (* step4: run the scan *)
  try
    (* TODO: call with:
       autofix=scan_handler.autofix if scan_handler else False,
       dryrun=True,
       # Always true, as we want to always report all findings, even
       # ignored ones, to the backend
       disable_nosem=True,
       baseline_commit=metadata.merge_base_ref,
       baseline_commit_is_mergebase=True,
    *)
    let profiler = Profiler.make () in
    (* TODO? use ci_conf.subdir at some point? *)
    let target_root : Scanning_root.t =
      (* alt: Sys.getcwd () *)
      Scanning_root.of_string "."
    in

    let targets_and_ignored =
      Find_targets.get_target_fpaths conf.targeting_conf [ target_root ]
    in
    let res =
      Scan_subcommand.check_targets_with_rules
        (* caps - network *)
        (caps
          :> < Cap.stdout
             ; Cap.chdir
             ; Cap.tmp
             ; Cap.fork
             ; Cap.time_limit
             ; Cap.memory_limit >)
        conf profiler rules_and_origin targets_and_ignored
    in
    match res with
    | Error e ->
        app.report_failure caps' ~scan_id e;
        Logs.err (fun m -> m "Encountered error when running rules");
        e
    | Ok (filtered_rules, _res, cli_output) ->
        (* step5: upload the findings *)
        let _cai_rules, blocking_rules, non_blocking_rules =
          partition_rules filtered_rules
        in
        let keep_ignored = false in
        (* TODO: the syntactic_id and match_based_id are hashes over parts of
           the finding.
           # Since we keep nosemgrep disabled for the actual scan, we have to apply
           # that flag here
           keep_ignored = not enable_nosem or output_handler.formatter.keep_ignores()
           for rule, matches in filtered_matches_by_rule.items():
             # Filter out any matches that are triaged as ignored on the app
             if scan_handler:
               matches = [
                 match
                 for match in matches
                 if match.syntactic_id not in scan_handler.skipped_syntactic_ids
                 and match.match_based_id not in scan_handler.skipped_match_based_ids
              ]
        *)
        let _cai_findings, blocking_findings, non_blocking_findings =
          partition_findings ~keep_ignored cli_output.results
        in

        (* TODO (output already called in Scan_subcommand.scan_files)
                            output_handler.output(
               {**blocking_matches_by_rule, **nonblocking_matches_by_rule},
               all_targets=output_extra.all_targets,
               ignore_log=ignore_log,
               profiler=profiler,
               filtered_rules=filtered_rules,
               profiling_data=output_extra.profiling_data,
               severities=shown_severities,
               is_ci_invocation=True,
               rules_by_engine=output_extra.rules_by_engine,
               engine_type=engine_type,
           )
        *)
        report_scan_completed ~blocking_findings ~blocking_rules
          ~non_blocking_findings ~non_blocking_rules;
        let app_block_override =
          upload_findings caps' app depl scan_id prj_meta blocking_findings
            filtered_rules cli_output
        in
        let audit_mode = false in
        (* TODO: audit_mode = metadata.event_name in audit_on *)
        exit_code_of_blocking_findings ~audit_mode ~on:prj_meta.on
          ~app_block_override blocking_findings
  with
  | Error.Semgrep_error (_, ex) as e ->
      let r = ex ||| Exit_code.fatal ~__LOC__ in
      app.report_failure caps' ~scan_id r;
      Logs.err (fun m ->
          m "Encountered error when running rules: %s" (Printexc.to_string e));
      let e = Exception.catch e in
      Exception.reraise e

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : < caps ; .. >) (argv : string array) : Exit_code.t =
  let conf = Ci_CLI.parse_argv argv in
  run_conf caps conf
