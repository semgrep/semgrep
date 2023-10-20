open Common
module Out = Semgrep_output_v1_j
module Http_helpers = Http_helpers.Make (Lwt_platform)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-ci command, execute it and exit.

   Translated from ci.py (and partially from scans.py)

   If 'semgrep ci' returns some networking errors, you may need to inspect
   the backend logs as the error message returned by the backend to the CLI
   might be short and may not contain the necessary information to debug.
   Even using --debug might not be enough.
   You can inspect the backend logs in Datadog and cloudwatch (and Metabase).
   However, it's probably better first to connect to the 'dev2' backend
   rather than 'prod' to have a lot less to search through.
   You can filter out by `env: dev2` in Datadog. To connect to dev2,
   you'll need to run semgrep ci like this:

     SEMGREP_APP_URL=https://dev2.semgrep.dev SEMGREP_APP_TOKEN=... semgrep ci

   Note that you'll first need to

      SEMGREP_APP_URL=https://dev2.semgrep.dev semgrep login

   as you'll need a separate app token for dev2. You can find the
   actual token value in your ~/.semgrep/settings.yml file

   Tip: you can store those environment variables in a dev2.sh env file
   that you can source instead.

   See https://www.notion.so/semgrep/Architecture-Overview-CI-Scans-afe6193a6cc84abd96cff5f2d91cecaa
   for an excellent overview of how 'semgrep ci' works with the backend.
   See also https://www.notion.so/semgrep/Scan-reliability-next-steps-Oct-2023-cf3dad02d1ff4e1a98db8acf7f7bbded
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

(* LATER: rewrite with 'match () with' instead of all those ifthenelse *)
let exit_code_of_blocking_findings ~audit_mode ~on ~app_block_override
    blocking_findings : Exit_code.t =
  let exit_code =
    if not (Common.null blocking_findings) then
      if audit_mode then (
        Logs.app (fun m ->
            m
              "  Audit mode is on for %s, so exiting with code 0 even if \
               matches found"
              on);
        Exit_code.ok)
      else (
        Logs.app (fun m ->
            m "  Has findings for blocking rules so exiting with code 1");
        Exit_code.findings)
    else (
      Logs.app (fun m -> m "  No blocking findings so exiting with code 0");
      Exit_code.ok)
  in
  match app_block_override with
  | Some reason when not audit_mode ->
      Logs.app (fun m ->
          m "  semgrep.dev is suggesting a non-zero exit code (%s)" reason);
      Exit_code.findings
  | _else_ -> exit_code

(* reports the failure for [scan_id] to Semgrep App *)
let report_failure ~dry_run ~token ~scan_id (exit_code : Exit_code.t) : unit =
  let int_code = Exit_code.to_int exit_code in
  if dry_run then
    Logs.app (fun m ->
        m "Would have reported failure to semgrep.dev: %u" int_code)
  else
    let uri =
      Uri.with_path !Semgrep_envvars.v.semgrep_url
        ("/api/agent/scans/" ^ scan_id ^ "/error")
    in
    let headers =
      [
        ("content-type", "application/json");
        ("authorization", "Bearer " ^ token);
      ]
    in
    let body =
      JSON.(
        string_of_json
          (Object [ ("exit_code", Int int_code); ("stderr", String "") ]))
    in
    match Http_helpers.post ~body ~headers uri with
    | Ok _ -> ()
    | Error (code, msg) ->
        Logs.err (fun m -> m "API server returned %u, this error: %s" code msg)

(*****************************************************************************)
(* Scan config *)
(*****************************************************************************)
(* token -> deployment_config -> scan_id -> scan_config -> rules *)

(* if something fails, we Error.exit *)
let deployment_config_opt (api_token : Auth.token option) (empty_config : bool)
    : (Auth.token * Semgrep_App.deployment_config) option =
  match (api_token, empty_config) with
  | None, true ->
      Logs.app (fun m ->
          m "run `semgrep login` before using `semgrep ci` or set `--config`");
      Error.exit Exit_code.invalid_api_key
  | Some _, false ->
      Logs.app (fun m ->
          m
            "Cannot run `semgrep ci` with --config while logged in. The \
             `semgrep ci` command will upload findings to semgrep-app and \
             those findings must come from rules configured there. Drop the \
             `--config` to use rules configured on semgrep.dev or log out.");
      Error.exit Exit_code.fatal
  (* TODO: document why we support running the ci command without a token *)
  | None, _ -> None
  | Some token, _ -> (
      match Semgrep_App.get_deployment_from_token ~token with
      | None ->
          Logs.app (fun m ->
              m
                "API token not valid. Try to run `semgrep logout` and `semgrep \
                 login` again.");
          Error.exit Exit_code.invalid_api_key
      | Some deployment_config -> Some (token, deployment_config))

(* eventually output the origin (if the semgrep_url is not semgrep.dev) *)
let at_url_maybe ppf () : unit =
  if
    Uri.equal !Semgrep_envvars.v.semgrep_url
      (Uri.of_string "https://semgrep.dev")
  then Fmt.string ppf ""
  else
    Fmt.pf ppf " at %a"
      Fmt.(styled `Bold string)
      (Uri.to_string !Semgrep_envvars.v.semgrep_url)

(* [data] contains the rules in JSON format. That's how the registry send
 * them because it's faster than using YAML.
 * TODO: factorize with Session.decode_rules()
 *)
let decode_json_rules (data : string) : Rule_fetching.rules_and_origin =
  Common2.with_tmp_file ~str:data ~ext:"json" (fun file ->
      let file = Fpath.v file in
      Rule_fetching.load_rules_from_file ~origin:Other_origin
        ~registry_caching:false file)

(* Either a scan_id and the rules for the project, or None and the rules
 * specified on command-line. If something fails, we Error.exit.
 *)
let scan_id_and_rules_from_deployment ~dry_run (prj_meta : Out.project_metadata)
    (token : Auth.token) (deployment_config : Semgrep_App.deployment_config) :
    Semgrep_App.scan_id * Rule_fetching.rules_and_origin list =
  Logs.app (fun m -> m "  %a" Fmt.(styled `Underline string) "CONNECTION");
  Logs.app (fun m ->
      m "  Reporting start of scan for %a"
        Fmt.(styled `Bold string)
        deployment_config.name);
  let scan_metadata : Out.scan_metadata =
    {
      cli_version = Version.version;
      unique_id = Uuidm.v `V4;
      (* TODO: should look at conf.secrets, conf.sca, conf.code, etc. *)
      requested_products = [];
    }
  in
  (* TODO:
      metadata_dict["is_sca_scan"] = supply_chain
      proj_config = ProjectConfig.load_all()
      metadata_dict = {**metadata_dict, **proj_config.to_dict()}
  *)
  match Semgrep_App.start_scan ~dry_run ~token prj_meta scan_metadata with
  | Error msg ->
      Logs.err (fun m -> m "Could not start scan %s" msg);
      Error.exit Exit_code.fatal
  | Ok scan_id ->
      (* TODO: should be concatenated with the "Reporting start ..." *)
      Logs.app (fun m -> m " (scan_id=%s)" scan_id);
      (* TODO: set sca to metadata.is_sca_scan / supply_chain *)
      let scan_config : Out.scan_config =
        Logs.app (fun m ->
            m "  Fetching configuration from Semgrep Cloud Platform%a"
              at_url_maybe ());
        match
          (* TODO: should pass and use scan_id *)
          Semgrep_App.fetch_scan_config ~token ~sca:false ~dry_run
            ~full_scan:prj_meta.is_full_scan ~repository:prj_meta.repository
        with
        | Error msg ->
            Logs.err (fun m -> m "Failed to download configuration: %s" msg);
            let r = Exit_code.fatal in
            report_failure ~dry_run ~token ~scan_id r;
            Error.exit r
        | Ok config -> config
      in
      let rules_and_origins =
        try decode_json_rules scan_config.rule_config with
        | Error.Semgrep_error (_, opt_ex) as e ->
            let ex = Option.value ~default:Exit_code.fatal opt_ex in
            report_failure ~dry_run ~token ~scan_id ex;
            let e = Exception.catch e in
            Exception.reraise e
      in
      (scan_id, [ rules_and_origins ])

(*****************************************************************************)
(* Project metadata *)
(*****************************************************************************)

(* from meta.py *)
let generate_meta_from_environment (baseline_ref : Digestif.SHA1.t option) :
    Project_metadata.t =
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
      (new Github_metadata.meta baseline_ref env gha_env)#project_metadata
  | _else ->
      let env = extract_env Git_metadata.env in
      (new Git_metadata.meta ~scan_environment:"git" ~baseline_ref env)
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
(* Partition rules *)
(*****************************************************************************)

let is_blocking (json : JSON.t) =
  match json with
  | JSON.Object xs -> (
      match List.assoc_opt "dev.semgrep.actions" xs with
      | Some (JSON.Array stuff) ->
          List.exists
            (function
              | JSON.String s -> String.equal s "block"
              | _else -> false)
            stuff
      | _else -> false)
  | _else -> false

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
           Option.value ~default:false (Option.map is_blocking r.Rule.metadata))
  in
  (cai_rules, blocking_rules, non_blocking_rules)

let partition_findings ~keep_ignored (results : Out.cli_match list) =
  let groups =
    results
    |> List.filter (fun (m : Out.cli_match) ->
           Option.value ~default:false m.Out.extra.Out.is_ignored
           && not keep_ignored)
    |> Common.group_by (fun (m : Out.cli_match) ->
           if
             Common2.string_match_substring
               (Str.regexp "r2c-internal-cai")
               (Rule_ID.to_string m.Out.check_id)
           then `Cai
           else if is_blocking (JSON.from_yojson m.Out.extra.Out.metadata) then
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

(* from rule_match.py *)
let severity_to_int (severity : Rule.severity) =
  match severity with
  | `Experiment -> `Int 4
  | `Warning -> `Int 1
  | `Error -> `Int 2
  | `Inventory
  | `Info ->
      `Int 0

(* this is used for sorting matches for findings *)
let ord_of_severity (severity : Rule.severity) : int =
  match severity with
  | `Experiment -> 0
  | `Inventory -> 1
  | `Info -> 2
  | `Warning -> 3
  | `Error -> 4

let finding_of_cli_match _commit_date index (m : Out.cli_match) : Out.finding =
  let (r : Out.finding) =
    Out.
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
        commit_date = "";
        (* TODO datetime.fromtimestamp(int(commit_date)).isoformat() *)
        syntactic_id = "";
        (* TODO, see rule_match.py *)
        match_based_id = None;
        (* TODO: see rule_match.py *)
        hashes = None;
        (* TODO should compute start_line_hash / end_line_hash / code_hash / pattern_hash *)
        metadata = m.extra.metadata;
        is_blocking = is_blocking (JSON.from_yojson m.Out.extra.Out.metadata);
        fixed_lines =
          None
          (* TODO: if self.extra.get("fixed_lines"): ret.fixed_lines = self.extra.get("fixed_lines") *);
        sca_info = None;
        (* TODO *)
        dataflow_trace = None;
        validation_state = None;
      }
  in
  r

(*****************************************************************************)
(* Reporting *)
(*****************************************************************************)

let report_scan_environment (prj_meta : Out.project_metadata) : unit =
  Logs.app (fun m -> m "  %a" Fmt.(styled `Underline string) "SCAN ENVIRONMENT");
  Logs.app (fun m ->
      m "  versions    - semgrep %a on OCaml %a"
        Fmt.(styled `Bold string)
        Version.version
        Fmt.(styled `Bold string)
        Sys.ocaml_version);
  Logs.app (fun m ->
      m "  environment - running in environment %a, triggering event is %a@."
        Fmt.(styled `Bold string)
        prj_meta.scan_environment
        Fmt.(styled `Bold string)
        prj_meta.on);
  ()

let report_scan_completed ~blocking_findings ~blocking_rules
    ~non_blocking_findings ~non_blocking_rules =
  Logs.app (fun m -> m "CI scan completed successfully.");
  Logs.app (fun m ->
      m "  Found %s (%u blocking) from %s."
        (String_utils.unit_str
           (List.length blocking_findings + List.length non_blocking_findings)
           "finding")
        (List.length blocking_findings)
        (String_utils.unit_str
           (List.length blocking_rules + List.length non_blocking_rules)
           "rule"));
  ()

(*****************************************************************************)
(* Uploading findings *)
(*****************************************************************************)

(* from scans.py *)
let findings_and_complete ~has_blocking_findings ~commit_date ~engine_requested
    (cli_output : Out.cli_output) (rules : Rule.rule list) :
    Out.ci_scan_results * Out.ci_scan_complete =
  let targets = cli_output.paths.scanned in
  let skipped = cli_output.paths.skipped in

  let rule_ids = rules |> Common.map (fun r -> fst r.Rule.id) in
  let contributions = Parse_contribution.get_contributions () in
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
    |> List.partition (fun m ->
           Option.value ~default:false m.Out.extra.Out.is_ignored)
  in
  let findings = Common.mapi (finding_of_cli_match commit_date) new_matches in
  let ignores = Common.mapi (finding_of_cli_match commit_date) new_ignored in
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
    |> Common.group_by (fun (skipped_target : Out.skipped_target) ->
           Fpath.get_ext skipped_target.Out.path)
    |> List.filter (fun (ext, _) -> not (String.equal ext ""))
    (* don't count files with no extension *)
    |> Common.map (fun (ext, xs) -> (ext, List.length xs))
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

let upload_findings ~dry_run
    (depl_opt : (string * Semgrep_App.deployment_config) option)
    (scan_id_opt : Semgrep_App.scan_id option) blocking_findings filtered_rules
    (cli_output : Out.cli_output) : Semgrep_App.app_block_override =
  match (depl_opt, scan_id_opt) with
  | Some (token, deployment_config), Some scan_id ->
      Logs.app (fun m -> m "  Uploading findings.");
      let results, complete =
        findings_and_complete
          ~has_blocking_findings:(not (Common.null blocking_findings))
          ~commit_date:"" ~engine_requested:`OSS cli_output filtered_rules
      in
      let override =
        match
          Semgrep_App.upload_findings ~token ~scan_id ~dry_run ~results
            ~complete
        with
        | Ok a -> a
        | Error msg ->
            Logs.err (fun m -> m "Failed to report findings: %s" msg);
            None
      in
      Logs.app (fun m -> m "  View results in Semgrep Cloud Platform:");
      Logs.app (fun m ->
          m "    https://semgrep.dev/orgs/%s/findings" deployment_config.name);
      if
        filtered_rules
        |> List.exists (fun r ->
               String.equal "r2c-internal-project-depends-on"
                 (Rule_ID.to_string (fst r.Rule.id)))
      then
        Logs.app (fun m ->
            m "    https://semgrep.dev/orgs/%s/supply-chain"
              deployment_config.name);
      override
  | _ -> None

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run_conf (ci_conf : Ci_CLI.conf) : Exit_code.t =
  let conf = ci_conf.scan_conf in
  (match conf.common.maturity with
  (* coupling: copy-pasted from Scan_subcommand.ml *)
  | Maturity.Default
    when conf.registry_caching || conf.core_runner_conf.ast_caching ->
      Error.abort "--registry_caching or --ast_caching require --experimental"
  | Maturity.Default -> (
      (* TODO: handle more confs, or fallback to pysemgrep further down *)
      match conf with
      | _else_ -> raise Pysemgrep.Fallback)
  | Maturity.Legacy -> raise Pysemgrep.Fallback
  | Maturity.Experimental
  | Maturity.Develop ->
      ());

  (* step1: initialization *)
  CLI_common.setup_logging ~force_color:conf.force_color
    ~level:conf.common.logging_level;
  (* TODO? we probably want to set the metrics to On by default in CI ctx? *)
  Metrics_.configure conf.metrics;
  let settings = Semgrep_settings.load ~maturity:conf.common.maturity () in
  Logs.debug (fun m -> m "conf = %s" (Ci_CLI.show_conf ci_conf));
  let dry_run = conf.dryrun in

  (* step2: token -> deployment_config -> scan_id -> scan_config -> rules *)
  let depl_opt =
    deployment_config_opt settings.api_token (conf.rules_source =*= Configs [])
  in
  (* TODO: pass baseline commit! *)
  let prj_meta = generate_meta_from_environment None in
  Logs.app (fun m -> m "%a" Fmt_helpers.pp_heading "Debugging Info");
  report_scan_environment prj_meta;
  (* TODO: fix_head_if_github_action(metadata) *)
  let scan_id_opt, rules_and_origin =
    match depl_opt with
    (* TODO: document why we support running the ci command without a
     * token / deployment. We could simplify the code.
     *)
    | None ->
        ( None,
          Rule_fetching.rules_from_rules_source ~token_opt:settings.api_token
            ~rewrite_rule_ids:conf.rewrite_rule_ids
            ~registry_caching:conf.registry_caching conf.rules_source )
    | Some (token, depl) ->
        let scan_id, rules =
          scan_id_and_rules_from_deployment ~dry_run prj_meta token depl
        in
        (Some scan_id, rules)
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

  (* step3: run the scan *)
  try
    (* TODO: call with:
       target = os.curdir
       autofix=scan_handler.autofix if scan_handler else False,
       dryrun=True,
       # Always true, as we want to always report all findings, even
       # ignored ones, to the backend
       disable_nosem=True,
       baseline_commit=metadata.merge_base_ref,
       baseline_commit_is_mergebase=True,
    *)
    let profiler = Profiler.make () in
    let targets_and_ignored =
      Find_targets.get_targets conf.targeting_conf conf.target_roots
    in
    let res =
      Scan_subcommand.run_scan_files conf profiler rules_and_origin
        targets_and_ignored
    in
    match res with
    | Error e ->
        (match (depl_opt, scan_id_opt) with
        | Some (token, _), Some scan_id ->
            report_failure ~dry_run ~token ~scan_id e
        | _else -> ());
        Logs.err (fun m -> m "Encountered error when running rules");
        e
    | Ok (filtered_rules, _res, cli_output) ->
        (* step4: upload the findings *)
        let _cai_rules, blocking_rules, non_blocking_rules =
          partition_rules filtered_rules
        in
        let keep_ignored = false in
        (* TODO: the syntactic_id and match_based_id are hashes over parts of the finding, not yet implemented in OCaml
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
          partition_findings ~keep_ignored cli_output.Out.results
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
          upload_findings ~dry_run depl_opt scan_id_opt blocking_findings
            filtered_rules cli_output
        in
        let audit_mode = false in
        (* TODO: audit_mode = metadata.event_name in audit_on *)
        exit_code_of_blocking_findings ~audit_mode ~on:prj_meta.on
          ~app_block_override blocking_findings
  with
  | Error.Semgrep_error (_, ex) as e ->
      (match (depl_opt, scan_id_opt) with
      | Some (token, _), Some scan_id ->
          let r = Option.value ~default:Exit_code.fatal ex in
          report_failure ~dry_run ~token ~scan_id r
      | _else -> ());
      Logs.err (fun m ->
          m "Encountered error when running rules: %s" (Printexc.to_string e));
      let e = Exception.catch e in
      Exception.reraise e

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Ci_CLI.parse_argv argv in
  run_conf conf
