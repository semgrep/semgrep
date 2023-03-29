(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-scan command, execute it and exit.

   Translated mainly from scan.py, with parts translated also
   from semgrep_main.py and core_runner.py.
*)

open File.Operators

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
let exit_code_of_errors ~strict (errors : Semgrep_output_v1_t.core_error list) :
    Exit_code.t =
  match List.rev errors with
  | [] -> Exit_code.ok
  (* TODO? why do we look at the last error? What about the other errors? *)
  | x :: _ -> (
      (* alt: raise a Semgrep_error that would be catched by CLI_Common
       * wrapper instead of returning an exit code directly? *)
      match () with
      | _ when x.severity = Semgrep_output_v1_t.Error ->
          Cli_json_output.exit_code_of_error_type x.error_type
      | _ when strict -> Cli_json_output.exit_code_of_error_type x.error_type
      | _else_ -> Exit_code.ok)

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
      let rules_and_origins =
        Rule_fetching.rules_from_rules_source conf.rules_source
      in
      Logs.debug (fun m ->
          rules_and_origins
          |> List.iter (fun x ->
                 m "rules = %s" (Rule_fetching.show_rules_and_origin x)));
      let rules, errors =
        Rule_fetching.partition_rules_and_errors rules_and_origins
      in
      let filtered_rules =
        Rule_filtering.filter_rules conf.rule_filtering_conf rules
      in
      let targets, semgrepignored_targets =
        Find_target.get_targets conf.targeting_conf conf.target_roots
      in
      Logs.debug (fun m ->
          m "target roots: [%s" "";
          conf.target_roots |> List.iter (fun root -> m "  %s" !!root);
          m "]%s" "");
      Logs.debug (fun m ->
          m "skipped targets: [%s" "";
          semgrepignored_targets
          |> List.iter (fun x ->
                 m "  %s" (Output_from_core_t.show_skipped_target x));
          m "]%s" "");
      Logs.debug (fun m ->
          m "selected targets: [%s" "";
          targets
          |> List.iter (fun file -> m "target = %s" (Fpath.to_string file));
          m "]%s" "");
      let (res : Core_runner.result) =
        Core_runner.invoke_semgrep_core conf.core_runner_conf filtered_rules
          errors targets
      in
      (* Add the targets that were semgrepignored *)
      let res =
        let core =
          {
            res.core with
            skipped_targets = semgrepignored_targets @ res.core.skipped_targets;
          }
        in
        { res with core }
      in
      (* TOPORT? was in formater/base.py
         def keep_ignores(self) -> bool:
           """
           Return True if ignored findings should be passed to this formatter;
           False otherwise.
           Ignored findings can still be distinguished using their _is_ignore property.
           """
           return False
      *)
      (* outputting the result! in JSON/Text/... depending on conf *)
      Output.output_result conf res;
      (* final result for the shell *)
      exit_code_of_errors ~strict:conf.strict res.core.errors

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Scan_CLI.parse_argv argv in
  run conf
