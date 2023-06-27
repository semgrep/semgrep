(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-scan command, execute it and exit.

   Translated mainly from scan.py, with parts translated also
   from semgrep_main.py and core_runner.py.
*)

module Out = Semgrep_output_v1_t
module RP = Report

(*****************************************************************************)
(* Logging/Profiling/Debugging *)
(*****************************************************************************)

let setup_logging (conf : Scan_CLI.conf) =
  CLI_common.setup_logging ~force_color:conf.force_color
    ~level:conf.logging_level;
  Logs.debug (fun m -> m "Semgrep version: %s" Version.version);
  ()

(* ugly: also partially done in CLI.ml *)
let setup_profiling (conf : Scan_CLI.conf) =
  (* TOADAPT
      if config.debug then Report.mode := MDebug
      else if config.report_time then Report.mode := MTime
      else Report.mode := MNo_info;
  *)
  if conf.profile then (
    (* ugly: no need to set Common.profile, this was done in CLI.ml *)
    Logs.debug (fun m -> m "Profile mode On");
    Logs.debug (fun m -> m "disabling -j when in profiling mode");
    { conf with core_runner_conf = { conf.core_runner_conf with num_jobs = 1 } })
  else conf

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

(* python: this used to be done in a _final_raise method from output.py
 * but better separation of concern to do it here.
 *)
let exit_code_of_errors ~strict (errors : Out.core_error list) : Exit_code.t =
  match List.rev errors with
  | [] -> Exit_code.ok
  (* TODO? why do we look at the last error? What about the other errors? *)
  | x :: _ -> (
      (* alt: raise a Semgrep_error that would be catched by CLI_Common
       * wrapper instead of returning an exit code directly? *)
      match () with
      | _ when x.severity = Out.Error ->
          Cli_json_output.exit_code_of_error_type x.error_type
      | _ when strict -> Cli_json_output.exit_code_of_error_type x.error_type
      | _else_ -> Exit_code.ok)

(*****************************************************************************)
(* Incremental display *)
(*****************************************************************************)

(* Note that this hook is run in parallel in Parmap at the end of processing
 * a file. Using Format.std_formatter in parallel requires some synchronization
 * to avoid having the output of multiple child processes interwinded, hence
 * the use of Unix.lockf below.
 *)
let file_match_results_hook (conf : Scan_CLI.conf) (rules : Rule.rules)
    (_file : Fpath.t) (match_results : RP.partial_profiling RP.match_result) :
    unit =
  let (cli_matches : Out.cli_match list) =
    (* need to go through a series of transformation so that we can
     * get something that Matches_report.pp_text_outputs can operate on
     *)
    let (pms : Pattern_match.t list) = match_results.matches in
    let (core_matches : Out.core_match list) =
      pms |> Common.partition_either (JSON_report.match_to_match None) |> fst
    in
    let hrules = Rule.hrules_of_rules rules in
    let env = { Cli_json_output.hrules } in
    core_matches
    |> Common.map (Cli_json_output.cli_match_of_core_match env)
    |> Cli_json_output.dedup_and_sort
  in
  (* TODO? needed? given the call to dedup_and_sort above? I just imitate
   * what we do in the regular code path
   *)
  let cli_matches = Matches_report.sort_matches cli_matches in
  (* TODO? not sure why the order in sort_matches is wrong *)
  let cli_matches = List.rev cli_matches in
  let cli_matches =
    cli_matches
    |> Common.exclude (fun m ->
           let to_ignore, _errs = Nosemgrep.rule_match_nosem ~strict:false m in
           to_ignore)
  in
  if cli_matches <> [] then (
    Unix.lockf Unix.stdout Unix.F_LOCK 0;
    (* coupling: similar to Output.dispatch_output_format for Text *)
    Matches_report.pp_text_outputs ~max_chars_per_line:conf.max_chars_per_line
      ~max_lines_per_finding:conf.max_lines_per_finding
      ~color_output:conf.force_color Format.std_formatter cli_matches;
    (* TODO? use finalize? *)
    Unix.lockf Unix.stdout Unix.F_ULOCK 0)

(*****************************************************************************)
(* Skipped analysis *)
(*****************************************************************************)

let errors_to_skipped (errors : Out.core_error list) : Out.skipped_target list =
  errors
  |> Common.map (fun Out.{ location; message; rule_id; _ } ->
         Out.
           {
             path = location.path;
             reason = Analysis_failed_parser_or_internal_error;
             details = message;
             rule_id;
           })

let analyze_skipped (skipped : Out.skipped_target list) =
  let groups =
    Common.group_by
      (fun (Out.{ reason; _ } : Out.skipped_target) ->
        match reason with
        | Out.Gitignore_patterns_match
        | Semgrepignore_patterns_match ->
            `Semgrepignore
        | Too_big
        | Exceeded_size_limit ->
            `Size
        | Cli_include_flags_do_not_match -> `Include
        | Cli_exclude_flags_match -> `Exclude
        | Always_skipped
        | Analysis_failed_parser_or_internal_error
        | Excluded_by_config
        | Wrong_language
        | Minified
        | Binary
        | Irrelevant_rule
        | Too_many_matches ->
            `Other)
      skipped
  in
  ( (try List.assoc `Semgrepignore groups with
    | Not_found -> []),
    (try List.assoc `Include groups with
    | Not_found -> []),
    (try List.assoc `Exclude groups with
    | Not_found -> []),
    (try List.assoc `Size groups with
    | Not_found -> []),
    try List.assoc `Other groups with
    | Not_found -> [] )

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (conf : Scan_CLI.conf) : Exit_code.t =
  setup_logging conf;
  (* return a new conf because can adjust conf.num_jobs (-j) *)
  let conf = setup_profiling conf in
  Logs.debug (fun m -> m "conf = %s" (Scan_CLI.show_conf conf));
  Metrics_.configure conf.metrics;
  let settings = Semgrep_settings.load ~legacy:conf.legacy () in

  match () with
  (* "alternate modes" where no search is performed.
   * coupling: if you add a new alternate mode, you probably need to modify
   * Scan_CLI.cmdline_term.combine.rules_source match cases and allow
   * more cases returning an empty 'Configs []'.
   * TODO? stricter: we should allow just one of those alternate modes.
   *)
  | _ when conf.version ->
      (* alt: we could use Common.pr, but because '--quiet' doc says
       * "Only output findings.", a version is not a finding so
       * we use Logs.app (which is filtered by --quiet).
       *)
      Logs.app (fun m -> m "%s" Version.version);
      (* TOPORT: if enable_version_check: version_check() *)
      Exit_code.ok
  | _ when conf.show_supported_languages ->
      Logs.app (fun m -> m "supported languages are: %s" Xlang.supported_xlangs);
      Exit_code.ok
  (* LATER: this should be real separate subcommands instead of abusing
   * semgrep scan flags
   *)
  | _ when conf.test <> None -> Test_subcommand.run (Common2.some conf.test)
  | _ when conf.validate <> None ->
      Validate_subcommand.run (Common2.some conf.validate)
  | _ when conf.dump <> None -> Dump_subcommand.run (Common2.some conf.dump)
  | _else_ ->
      (* --------------------------------------------------------- *)
      (* Let's go *)
      (* --------------------------------------------------------- *)
      (* step0: potentially notify user about metrics *)
      if not (settings.has_shown_metrics_notification = Some true) then (
        (* python compatibility: the 22m and 24m are "normal color or intensity", and "underline off" *)
        let esc =
          if Fmt.style_renderer Fmt.stderr = `Ansi_tty then "\027[22m\027[24m"
          else ""
        in
        Logs.warn (fun m ->
            m
              "%sMETRICS: Using configs from the Registry (like --config=p/ci) \
               reports pseudonymous rule metrics to semgrep.dev.@.To disable \
               Registry rule metrics, use \"--metrics=off\".@.Using configs \
               only from local files (like --config=xyz.yml) does not enable \
               metrics.@.@.More information: https://semgrep.dev/docs/metrics"
              esc);
        Logs.app (fun m -> m "");
        let settings =
          {
            settings with
            Semgrep_settings.has_shown_metrics_notification = Some true;
          }
        in
        ignore (Semgrep_settings.save settings));

      (* step1: getting the rules *)

      (* Rule_fetching.rules_and_origin record also contain errors *)
      let rules_and_origins =
        Rule_fetching.rules_from_rules_source ~token_opt:settings.api_token
          ~rewrite_rule_ids:conf.rewrite_rule_ids
          ~registry_caching:conf.registry_caching conf.rules_source
      in
      let rules, errors =
        Rule_fetching.partition_rules_and_errors rules_and_origins
      in
      (* TODO: we should probably warn the user about rules using the same id *)
      let rules =
        Common.uniq_by
          (fun r1 r2 -> Rule.ID.equal (fst r1.Rule.id) (fst r2.Rule.id))
          rules
      in
      if Common.null rules then Exit_code.missing_config
      else
        let filtered_rules =
          Rule_filtering.filter_rules conf.rule_filtering_conf rules
        in
        Logs.info (fun m ->
            m "%a" Rules_report.pp_rules (conf.rules_source, filtered_rules));

        (* step2: getting the targets *)
        let targets, semgrepignored_targets =
          Find_targets.get_targets conf.targeting_conf conf.target_roots
        in
        Logs.debug (fun m ->
            m "%a" Targets_report.pp_targets_debug
              (conf.target_roots, semgrepignored_targets, targets));
        Logs.info (fun m ->
            semgrepignored_targets
            |> List.iter (fun (x : Output_from_core_t.skipped_target) ->
                   m "Ignoring %s due to %s (%s)" x.Output_from_core_t.path
                     (Output_from_core_t.show_skip_reason
                        x.Output_from_core_t.reason)
                     x.Output_from_core_t.details));

        (* step3: running the engine *)
        let output_format, file_match_results_hook =
          match conf with
          | { output_format = Output_format.Text; legacy = false; _ } ->
              ( Output_format.TextIncremental,
                Some (file_match_results_hook conf filtered_rules) )
          | { output_format; _ } -> (output_format, None)
        in
        let (res : Core_runner.result) =
          Core_runner.invoke_semgrep_core
            ~matching_explanations:conf.matching_explanations
            ~respect_git_ignore:conf.targeting_conf.respect_git_ignore
            ~file_match_results_hook conf.core_runner_conf filtered_rules errors
            targets
        in

        (* step4: report matches *)
        let errors_skipped = errors_to_skipped res.core.errors in
        let semgrepignored, included, excluded, size, other_ignored =
          analyze_skipped semgrepignored_targets
        in
        let res =
          let skipped_targets =
            Some
              (semgrepignored_targets @ errors_skipped
              @ Common.optlist_to_list res.core.skipped_targets)
          in
          (* Add the targets that were semgrepignored or errorneous *)
          let core = { res.core with skipped_targets } in
          { res with core }
        in

        (* outputting the result! in JSON/Text/... depending on conf *)
        let cli_output = Output.output_result { conf with output_format } res in

        Logs.info (fun m ->
            m "%a" Skipped_report.pp_skipped
              ( conf.targeting_conf.respect_git_ignore,
                conf.legacy,
                conf.targeting_conf.max_target_bytes,
                semgrepignored,
                included,
                excluded,
                size,
                other_ignored,
                errors_skipped ));
        Logs.app (fun m ->
            m "%a" Summary_report.pp_summary
              ( conf.targeting_conf.respect_git_ignore,
                conf.legacy,
                conf.targeting_conf.max_target_bytes,
                semgrepignored,
                included,
                excluded,
                size,
                other_ignored,
                errors_skipped ));
        Logs.app (fun m ->
            m "Ran %s on %s: %s."
              (String_utils.unit_str (List.length filtered_rules) "rule")
              (String_utils.unit_str
                 (List.length cli_output.paths.scanned)
                 "file")
              (String_utils.unit_str (List.length cli_output.results) "finding"));

        (* TOPORT? was in formater/base.py
           def keep_ignores(self) -> bool:
             """
             Return True if ignored findings should be passed to this formatter;
             False otherwise.
             Ignored findings can still be distinguished using their _is_ignore property.
             """
             return False
        *)

        (* step5: exit with the right exit code *)
        (* final result for the shell *)
        if conf.error_on_findings && not (Common.null cli_output.results) then
          Exit_code.findings
        else exit_code_of_errors ~strict:conf.strict res.core.errors

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Scan_CLI.parse_argv argv in
  run conf
