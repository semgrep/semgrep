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

(* ugly: also partially done in CLI.ml *)
let setup_logging (conf : Scan_CLI.conf) =
  (* For osemgrep we use the Logs library instead of the Logger
   * library in pfff. We had a few issues with Logger (which is a small
   * wrapper around the easy_logging library), and we don't really want
   * the logging in semgrep-core to interfere with the proper
   * logging/output we want in osemgrep, so this is a good opportunity
   * to evaluate a new logging library.
   *)
  Logs_helpers.setup_logging ~force_color:conf.force_color
    ~level:conf.logging_level;
  (* TOPORT
        # Setup file logging
        # env.user_log_file dir must exist
        env.user_log_file.parent.mkdir(parents=True, exist_ok=True)
        file_handler = logging.FileHandler(env.user_log_file, "w")
        file_formatter = logging.Formatter(
            "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
        )
        file_handler.setLevel(logging.DEBUG)
        file_handler.setFormatter(file_formatter)
        logger.addHandler(file_handler)
  *)
  Logs.debug (fun m -> m "Logging setup for semgrep scan");
  Logs.debug (fun m -> m "Semgrep version: %s" Version.version);
  Logs.debug (fun m ->
      m "Executed as: %s" (Sys.argv |> Array.to_list |> String.concat " "));

  (* Easy_logging setup. We should avoid to use Logger in osemgrep/
   * and use Logs instead, but it is still useful to get the semgrep-core
   * logging information at runtime, hence this call.
   *)
  let debug =
    match conf.logging_level with
    | Some Logs.Debug -> true
    | _else_ -> false
  in
  Logging_helpers.setup ~debug
    ~log_config_file:(Fpath.v "log_config.json")
    ~log_to_file:None;
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

let exit_code_of_cli_errors (errors : Out.cli_error list) : Exit_code.t =
  match List.rev errors with
  | [] -> Exit_code.ok
  | x :: _ -> Exit_code.of_int x.Out.code

(*****************************************************************************)
(* Incremental display *)
(*****************************************************************************)

(* Note that this is run in parallel in Parmap at the end of processing
 * a file.
 * TODO: Using Format.std_formatter in parallel is not good; the output
 * is interwinded. Need to find a way to synchronize them. At least
 * it works with osemgrep -j 1
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
  if cli_matches <> [] then
    (* coupling: similar to Output.dispatch_output_format for Text *)
    Matches_report.pp_text_outputs ~max_chars_per_line:conf.max_chars_per_line
      ~max_lines_per_finding:conf.max_lines_per_finding
      ~color_output:conf.force_color Format.std_formatter cli_matches

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
  let settings = Semgrep_settings.load () in

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
  | _else_ -> (
      (* --------------------------------------------------------- *)
      (* Let's go *)
      (* --------------------------------------------------------- *)
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
      let filtered_rules =
        Rule_filtering.filter_rules conf.rule_filtering_conf rules
      in
      Logs.info (fun m ->
          m "%a" Rules_report.pp_rules (conf.rules_source, filtered_rules));

      (* step2: getting the targets *)
      let targets, semgrepignored_targets =
        Find_target.get_targets conf.targeting_conf conf.target_roots
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
          semgrepignored_targets @ errors_skipped @ res.core.skipped_targets
        in
        (* Add the targets that were semgrepignored or errorneous *)
        let core = { res.core with skipped_targets } in
        { res with core }
      in

      (* outputting the result! in JSON/Text/... depending on conf *)
      let cli_errors = Output.output_result { conf with output_format } res in

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
              conf.targeting_conf.max_target_bytes,
              semgrepignored,
              included,
              excluded,
              size,
              other_ignored,
              errors_skipped ));
      Logs.app (fun m ->
          m "Ran %d rules on %d files: %d findings@."
            (List.length filtered_rules)
            (List.length targets)
            (List.length res.core.matches));

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
      match cli_errors with
      | [] ->
          (* final result for the shell *)
          exit_code_of_errors ~strict:conf.strict res.core.errors
      | errors -> exit_code_of_cli_errors errors)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Scan_CLI.parse_argv argv in
  run conf
