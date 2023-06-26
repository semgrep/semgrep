(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-ci command, execute it and exit.

   Translated from ci.py
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* from meta.py *)
let generate_meta_from_environment (_baseline_ref : Digestif.SHA1.t option) :
    unit Project_metadata.t =
  (* https://help.github.com/en/actions/configuring-and-managing-workflows/using-environment-variables *)
  let r =
    let argv = [| "empty" |] and info_ = Cmdliner.Cmd.info "" in
    match Sys.getenv_opt "GITHUB_ACTIONS" with
    | Some "true" ->
        (* TODO baseline_ref *)
        Cmdliner.Cmd.(eval_value ~argv (v info_ Github_metadata.term))
    | _else ->
        (* TODO baseline_ref *)
        Cmdliner.Cmd.(eval_value ~argv (v info_ Git_metadata.term))
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
  in
  match r with
  | Ok (`Ok a) -> a
  | Ok `Version
  | Ok `Help ->
      invalid_arg "unexpected version or help"
  | Error _e -> invalid_arg "couldn't decode environment"

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (conf : Ci_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:conf.force_color
    ~level:conf.logging_level;
  Metrics_.configure conf.metrics;
  let settings = Semgrep_settings.load ~legacy:conf.legacy () in
  let deployment =
    match (settings.api_token, conf.rules_source) with
    | None, Rules_source.Configs [] ->
        Logs.app (fun m ->
            m "run `semgrep login` before using `semgrep ci` or set `--config`");
        Error Exit_code.invalid_api_key
    | Some _, Rules_source.Configs (_ :: _) ->
        Logs.app (fun m ->
            m
              "Cannot run `semgrep ci` with --config while logged in. The \
               `semgrep ci` command will upload findings to semgrep-app and \
               those findings must come from rules configured there. Drop the \
               `--config` to use rules configured on semgrep.dev or log out.");
        Error Exit_code.fatal
    | None, _ -> Ok None
    | Some token, _ -> (
        match Semgrep_App.get_deployment_from_token ~token with
        | None ->
            Logs.app (fun m ->
                m
                  "API token not valid. Try to run `semgrep logout` and \
                   `semgrep login` again.");
            Error Exit_code.invalid_api_key
        | Some t -> Ok (Some (token, t)))
  in
  (* TODO: pass baseline commit! *)
  let metadata = generate_meta_from_environment None in
  match deployment with
  | Error e -> e
  | Ok depl ->
      Logs.app (fun m -> m "%a" Fmt_helpers.pp_heading "Debugging Info");
      Logs.app (fun m ->
          m "  %a" Fmt.(styled `Underline string) "SCAN ENVIRONMENT");
      Logs.app (fun m ->
          m "  versions    - semgrep %a on OCaml %a"
            Fmt.(styled `Bold string)
            Version.version
            Fmt.(styled `Bold string)
            Sys.ocaml_version);
      Logs.app (fun m ->
          m
            "  environment - running in environment %a, triggering event is \
             %a@."
            Fmt.(styled `Bold string)
            (Option.value ~default:"unknown"
               metadata.Project_metadata.scan_environment)
            Fmt.(styled `Bold string)
            (Option.value ~default:"unknown" metadata.Project_metadata.on));
      (* TODO: fix_head_if_github_action(metadata) *)
      let _scan_id, _rules_and_origins =
        match depl with
        | None ->
            ( None,
              Rule_fetching.rules_from_rules_source
                ~token_opt:settings.api_token
                ~rewrite_rule_ids:conf.rewrite_rule_ids
                ~registry_caching:conf.registry_caching conf.rules_source )
        | Some (token, deployment) ->
            Logs.app (fun m ->
                m "  %a" Fmt.(styled `Underline string) "CONNECTION");
            Logs.app (fun m ->
                m "  Reporting start of scan for %a"
                  Fmt.(styled `Bold string)
                  deployment);
            let metadata_dict = Project_metadata.to_dict metadata in
            (* TODO: metadata_dict["is_sca_scan"] = supply_chain *)
            (* TODO: proj_config = ProjectConfig.load_all()
               metadata_dict = {**metadata_dict, **proj_config.to_dict()} *)
            let scan_id =
              Scan_helper.start_scan ~dry_run:conf.dryrun ~token
                Semgrep_envvars.v.semgrep_url metadata_dict
            in
            let at_url_maybe ppf () =
              if
                Uri.equal Semgrep_envvars.v.semgrep_url
                  (Uri.of_string "https://semgrep.dev")
              then Fmt.string ppf ""
              else
                Fmt.pf ppf " at %a"
                  Fmt.(styled `Bold string)
                  (Uri.to_string Semgrep_envvars.v.semgrep_url)
            in
            Logs.app (fun m ->
                m "  Fetching configuration from Semgrep Cloud Platform%a"
                  at_url_maybe ());
            let rules =
              (* TODO: set sca to metadata.is_sca_scan / supply_chain *)
              Scan_helper.fetch_scan_config ~token ~sca:false
                ~dry_run:conf.dryrun ~full_scan:metadata.is_full_scan
                metadata.repository
            in
            ( Some scan_id,
              [
                Common2.with_tmp_file ~str:rules ~ext:"json" (fun file ->
                    let file = Fpath.v file in
                    let res =
                      Rule_fetching.load_rules_from_file ~registry_caching:false
                        file
                    in
                    { res with origin = None });
              ] )
      in
      (* TODO: do the actual scan, and reporting *)
      Exit_code.ok

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Ci_CLI.parse_argv argv in
  run conf
