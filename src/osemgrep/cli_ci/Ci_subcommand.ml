(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-ci command, execute it and exit.

   Translated from ci.py
*)

module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* eventually output the origin (if the semgrep_url is not semgrep.dev) *)
let at_url_maybe ppf () =
  if
    Uri.equal Semgrep_envvars.v.semgrep_url
      (Uri.of_string "https://semgrep.dev")
  then Fmt.string ppf ""
  else
    Fmt.pf ppf " at %a"
      Fmt.(styled `Bold string)
      (Uri.to_string Semgrep_envvars.v.semgrep_url)

let decode_rules data =
  Common2.with_tmp_file ~str:data ~ext:"json" (fun file ->
      let file = Fpath.v file in
      let res =
        Rule_fetching.load_rules_from_file ~registry_caching:false file
      in
      { res with origin = None })

let fetch_scan_config ~token ~dry_run ~sca ~full_scan ~repository scan_id =
  Logs.app (fun m ->
      m "  Fetching configuration from Semgrep Cloud Platform%a" at_url_maybe ());
  match
    Scan_helper.fetch_scan_config ~token ~sca ~dry_run ~full_scan repository
  with
  | Error msg ->
      Logs.err (fun m -> m "Failed to download configuration: %s" msg);
      let r = Exit_code.fatal in
      ignore
        (Scan_helper.report_failure ~dry_run ~token ~scan_id
           (Exit_code.to_int r));
      Error r
  | Ok rules ->
      let rules_and_origins =
        try decode_rules rules with
        | Error.Semgrep_error (_, opt_ex) as e ->
            let ex = Option.value ~default:Exit_code.fatal opt_ex in
            ignore
              (Scan_helper.report_failure ~dry_run ~token ~scan_id
                 (Exit_code.to_int ex));
            let e = Exception.catch e in
            Exception.reraise e
      in
      Ok [ rules_and_origins ]

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
    List.partition
      (fun r ->
        Common2.string_match_substring
          (Str.regexp "r2c-internal-cai")
          (Rule.ID.to_string (fst r.Rule.id)))
      filtered_rules
  in
  let blocking_rules, non_blocking_rules =
    List.partition
      (fun r ->
        Option.value ~default:false (Option.map is_blocking r.Rule.metadata))
      rest
  in
  (cai_rules, blocking_rules, non_blocking_rules)

let partition_findings ~keep_ignored (results : Out.cli_match list) =
  let groups =
    List.filter
      (fun (m : Out.cli_match) ->
        Option.value ~default:false m.Out.extra.Out.is_ignored
        && not keep_ignored)
      results
    |> Common.group_by (fun (m : Out.cli_match) ->
           if
             Common2.string_match_substring
               (Str.regexp "r2c-internal-cai")
               m.Out.check_id
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

(* from rule_match.py *)
let severity_to_int = function
  | "EXPERIMENT" -> `Int 4
  | "WARNING" -> `Int 1
  | "ERROR" -> `Int 2
  | _ -> `Int 0

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
        message = m.Out.extra.Out.message;
        severity = severity_to_int m.Out.extra.Out.severity;
        index;
        commit_date = "";
        (* TODO datetime.fromtimestamp(int(commit_date)).isoformat() *)
        syntactic_id = "";
        (* TODO, see rule_match.py *)
        match_based_id = None;
        (* TODO: see rule_match.py *)
        hashes = None;
        (* TODO should compute start_line_hash / end_line_hash / code_hash / pattern_hash *)
        metadata = m.Out.extra.Out.metadata;
        is_blocking = is_blocking (JSON.from_yojson m.Out.extra.Out.metadata);
        fixed_lines =
          None
          (* TODO: if self.extra.get("fixed_lines"): ret.fixed_lines = self.extra.get("fixed_lines") *);
        sca_info = None;
        (* TODO *)
        dataflow_trace = None (* TODO *);
      }
  in
  r

(* from scans.py *)
let prepare_for_report ~blocking_findings findings errors rules ~targets
    ~(ignored_targets : Out.cli_skipped_target list option) ~commit_date
    ~engine_requested =
  let rule_ids =
    Common.map (fun r -> Rule.ID.to_string (fst r.Rule.id)) rules
  in
  (*
      we want date stamps assigned by the app to be assigned such that the
      current sort by relevant_since results in findings within a given scan
      appear in an intuitive order.  this requires reversed ordering here.
     *)
  let all_matches = List.rev findings in
  let all_matches =
    let to_int = function
      | "EXPERIMENT" -> 0
      | "INVENTORY" -> 1
      | "INFO" -> 2
      | "WARNING" -> 3
      | "ERROR" -> 4
      | _ -> invalid_arg "unknown severity"
    in
    let sort_severity a b = Int.compare (to_int a) (to_int b) in
    List.sort
      (fun m1 m2 ->
        sort_severity m1.Out.extra.Out.severity m2.Out.extra.Out.severity)
      all_matches
  in
  let new_ignored, new_matches =
    List.partition
      (fun m -> Option.value ~default:false m.Out.extra.Out.is_ignored)
      all_matches
  in
  let findings = Common.mapi (finding_of_cli_match commit_date) new_matches
  and ignores = Common.mapi (finding_of_cli_match commit_date) new_ignored in
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
  let ci_scan_results =
    Out.
      {
        (* send a backup token in case the app is not available *)
        findings;
        ignores;
        token = ci_token;
        searched_paths = List.sort String.compare targets;
        (* TODO: get renamed_paths, depends on baseline_commit *)
        renamed_paths = [];
        rule_ids;
      }
  in
  let findings_and_ignores =
    JSON.json_of_string
    @@ Semgrep_output_v1_j.string_of_ci_scan_results ci_scan_results
  in
  if
    List.exists
      (fun m -> String.equal m.Out.extra.severity "EXPERIMENT")
      new_ignored
  then
    Logs.app (fun m -> m "Some experimental rules were run during execution.");

  let ignored_ext_freqs =
    Option.value ~default:[] ignored_targets
    |> Common.group_by (fun (skipped_target : Out.cli_skipped_target) ->
           Fpath.get_ext (Fpath.v skipped_target.Out.path))
    |> List.filter (fun (ext, _) -> not (String.equal ext ""))
    (* don't count files with no extension *)
    |> Common.map (fun (ext, xs) -> (ext, JSON.Int (List.length xs)))
  in

  (* dependency_counts = {k: len(v) for k, v in lockfile_dependencies.items()} *)

  (* TODO: add this data structure to the semgrep_output_v1.atd spec
     POST to /api/agent/scans/<scan_id>/complete *)
  let complete =
    let errors =
      Common.map
        (fun e ->
          JSON.json_of_string (Semgrep_output_v1_j.string_of_cli_error e))
        errors
    in
    JSON.Object
      [
        ("exit_code", if blocking_findings then JSON.Int 1 else JSON.Int 0);
        ("dependency_parser_errors", JSON.Array []);
        (* [e.to_json() for e in dependency_parser_errors], *)
        ( "stats",
          JSON.Object
            [
              ("findings", JSON.Int (List.length new_matches));
              ("errors", JSON.Array errors);
              ("total_time", JSON.Float 0.0);
              (* total_time *)
              ("unsupported_exts", JSON.Object ignored_ext_freqs);
              ("lockfile_scan_info", JSON.Object []);
              (* dependency_counts *)
              ("parse_rate", JSON.Object [])
              (*
  lang: {
    "targets_parsed": data.num_targets - data.targets_with_errors,
                      "num_targets": data.num_targets,
                      "bytes_parsed": data.num_bytes - data.error_bytes,
                      "num_bytes": data.num_bytes,
  }
      for (lang, data) in parse_rate.get_errors_by_lang().items() *);
            ] );
        ( "engine_requested",
          JSON.String
            (Semgrep_output_v1_j.string_of_engine_kind engine_requested) );
        ("dependencies", JSON.Object [])
        (*
             if self._dependency_query:
                 lockfile_dependencies_json = {}
                 for path, dependencies in lockfile_dependencies.items():
                     lockfile_dependencies_json[path] = [
                         dependency.to_json() for dependency in dependencies
                     ]
                 complete["dependencies"] = lockfile_dependencies_json
     *);
      ]
  in
  (findings_and_ignores, complete)

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
  | Ok depl -> (
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
      (* Either a scan_id and the rules for the project, or None and the rules
         specified on command-line. If something fails, an exit code is
         returned. *)
      let (r
            : ( string option * Rule_fetching.rules_and_origin list,
                Exit_code.t )
              result) =
        match depl with
        | None ->
            Ok
              ( None,
                Rule_fetching.rules_from_rules_source
                  ~token_opt:settings.api_token
                  ~rewrite_rule_ids:conf.rewrite_rule_ids
                  ~registry_caching:conf.registry_caching conf.rules_source )
        | Some (token, deployment) -> (
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
            match
              Scan_helper.start_scan ~dry_run:conf.dryrun ~token
                Semgrep_envvars.v.semgrep_url metadata_dict
            with
            | Error msg ->
                Logs.err (fun m -> m "Could not start scan %s" msg);
                Error Exit_code.fatal
            | Ok scan_id ->
                (* TODO: set sca to metadata.is_sca_scan / supply_chain *)
                Result.map
                  (fun r -> (Some scan_id, r))
                  (fetch_scan_config ~token ~dry_run:conf.dryrun ~sca:false
                     ~full_scan:metadata.is_full_scan
                     ~repository:metadata.repository scan_id))
      in

      match r with
      | Error ex -> ex
      | Ok (scan_id, rules_and_origin) -> (
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
            match Scan_subcommand.scan_files rules_and_origin profiler conf with
            | Error e ->
                (match (depl, scan_id) with
                | Some (token, _), Some scan_id ->
                    ignore
                      (Scan_helper.report_failure ~dry_run:conf.dryrun ~token
                         ~scan_id (Exit_code.to_int e))
                | _else -> ());
                Logs.err (fun m -> m "Encountered error when running rules");
                e
            | Ok (filtered_rules, _res, cli_output) ->
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
                Logs.app (fun m -> m "CI scan completed successfully.");
                Logs.app (fun m ->
                    m "  Found %s (%u blocking) from %s."
                      (String_utils.unit_str
                         (List.length blocking_findings
                         + List.length non_blocking_findings)
                         "finding")
                      (List.length blocking_findings)
                      (String_utils.unit_str
                         (List.length blocking_rules
                         + List.length non_blocking_rules)
                         "rule"));
                let app_block_override, reason =
                  match (depl, scan_id) with
                  | Some (token, deployment_name), Some scan_id ->
                      Logs.app (fun m -> m "  Uploading findings.");
                      let findings_and_ignores, complete =
                        prepare_for_report
                          ~blocking_findings:
                            (not (Common.null blocking_findings))
                          cli_output.Out.results cli_output.Out.errors
                          filtered_rules
                          ~targets:cli_output.Out.paths.Out.scanned
                          ~ignored_targets:cli_output.Out.paths.skipped
                          ~commit_date:"" ~engine_requested:`OSS
                      in
                      let result =
                        match
                          Scan_helper.report_findings ~token ~scan_id
                            ~dry_run:conf.dryrun ~findings_and_ignores ~complete
                        with
                        | Ok a -> a
                        | Error msg ->
                            Logs.err (fun m ->
                                m "Failed to report findings: %s" msg);
                            (false, "")
                      in
                      Logs.app (fun m ->
                          m "  View results in Semgrep Cloud Platform:");
                      Logs.app (fun m ->
                          m "    https://semgrep.dev/orgs/%s/findings"
                            deployment_name);
                      if
                        List.exists
                          (fun r ->
                            String.equal "r2c-internal-project-depends-on"
                              (Rule.ID.to_string (fst r.Rule.id)))
                          filtered_rules
                      then
                        Logs.app (fun m ->
                            m "    https://semgrep.dev/orgs/%s/supply-chain"
                              deployment_name);
                      result
                  | _ -> (false, "")
                in
                let audit_mode = false in
                (* TODO: audit_mode = metadata.event_name in audit_on *)
                let exit_code =
                  if not (Common.null blocking_findings) then
                    if audit_mode then (
                      Logs.app (fun m ->
                          m
                            "  Audit mode is on for %s, so exiting with code 0 \
                             even if matches found"
                            (Option.value ~default:"unknown"
                               metadata.Project_metadata.on));
                      Exit_code.ok)
                    else (
                      Logs.app (fun m ->
                          m
                            "  Has findings for blocking rules so exiting with \
                             code 1");
                      Exit_code.findings)
                  else (
                    Logs.app (fun m ->
                        m "  No blocking findings so exiting with code 0");
                    Exit_code.ok)
                in
                if app_block_override && not audit_mode then (
                  Logs.app (fun m ->
                      m "  semgrep.dev is suggesting a non-zero exit code (%s)"
                        reason);
                  Exit_code.findings)
                else exit_code
          with
          | Error.Semgrep_error (_, ex) as e ->
              (match (depl, scan_id) with
              | Some (token, _), Some scan_id ->
                  let r = Option.value ~default:Exit_code.fatal ex in
                  ignore
                    (Scan_helper.report_failure ~dry_run:conf.dryrun ~token
                       ~scan_id (Exit_code.to_int r))
              | _else -> ());
              Logs.err (fun m ->
                  m "Encountered error when running rules: %s"
                    (Printexc.to_string e));
              let e = Exception.catch e in
              Exception.reraise e))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Ci_CLI.parse_argv argv in
  run conf
