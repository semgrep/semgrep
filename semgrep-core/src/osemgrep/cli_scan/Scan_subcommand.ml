(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-scan command, execute it and exit.

   Translated from scan.py and semgrep_main.py
*)

(*****************************************************************************)
(* Logging/Profiling/Debugging *)
(*****************************************************************************)

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
  Logs.debug (fun m -> m "conf = %s" (Scan_CLI.show_conf conf));

  (* Easy_logging setup. We should avoid to use Logger in osemgrep/
   * and use Logs instead, but it is still useful to get the semgrep-core
   * logging information at runtime, hence this call.
   *)
  let config = Core_runner.runner_config_of_conf conf in
  Setup_logging.setup config;
  ()

(* TODO *)
let setup_profiling conf =
  (* LATER? this could be done in CLI.ml too *)

  (* TOADAPT
      if config.debug then Report.mode := MDebug
      else if config.report_time then Report.mode := MTime
      else Report.mode := MNo_info;
     ...

     let config =
        if config.profile then (
          logger#info "Profile mode On";
          logger#info "disabling -j when in profiling mode";
          { config with ncores = 1 })
        else config
      in
  *)
  conf

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

  match () with
  (* "alternate modes" where no search is performed *)
  | _ when conf.version ->
      (* alt: we could use Common.pr, but because '--quiet' doc says
       * "Only output findings.", a version is not a finding so
       * we use Logs.app (which is filtered by --quiet).
       *)
      Logs.app (fun m -> m "%s" Version.version);
      (* TOPORT:
         if enable_version_check:
               from semgrep.app.version import version_check
               version_check()
      *)
      Exit_code.ok
  | _ when conf.show_supported_languages ->
      Logs.app (fun m -> m "supported languages are: %s" Xlang.supported_xlangs);
      Exit_code.ok
  | _else_ ->
      (* --------------------------------------------------------- *)
      (* Let's go *)
      (* --------------------------------------------------------- *)
      (* TODO: in theory we should have an intermediate module that
       * handle the -e/--lang, or --config, but for now we care
       * only about --config.
       *)
      let rules_and_origins =
        conf.config
        |> List.concat_map Config_resolver.rules_from_dashdash_config
      in
      (* TODO: rewrite rule_id using x.Config_resolver.path origin? *)
      let (rules : Rule.rules) =
        rules_and_origins |> List.concat_map (fun x -> x.Config_resolver.rules)
      in
      let (errors : Rule.invalid_rule_error list) =
        rules_and_origins |> List.concat_map (fun x -> x.Config_resolver.errors)
      in

      let filtered_rules =
        match conf.severity with
        | [] -> rules
        | xs ->
            rules
            |> List.filter (fun r ->
                   match
                     Severity.rule_severity_of_rule_severity_opt r.Rule.severity
                   with
                   | None -> false
                   | Some x -> List.mem x xs)
      in
      let filtered_rules =
        filtered_rules
        |> Common.exclude (fun r ->
               List.mem (fst r.Rule.id) conf.exclude_rule_ids)
      in
      (* TODO: there are more ways to specify targets? see target_manager.py *)
      let (targets : Common.filename list), _skipped_targetsTODO =
        Find_target.select_global_targets ~includes:conf.include_
          ~excludes:conf.exclude ~max_target_bytes:conf.max_target_bytes
          ~respect_git_ignore:conf.respect_git_ignore conf.target_roots
      in
      let (res : Core_runner.result) =
        Core_runner.invoke_semgrep_core conf filtered_rules errors targets
      in
      (* outputting the result! in JSON/Text/... depending on conf *)
      Output.output_result conf res;
      (* final result for the shell *)
      exit_code_of_errors ~strict:conf.strict res.core.errors

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let res = Scan_CLI.parse_argv argv in
  (* LATER: this error handling could be factorized
   * between the different subcommands at some point
   *)
  match res with
  | Ok conf -> run conf
  | Error exit_code -> exit_code
