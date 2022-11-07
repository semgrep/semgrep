(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-scan command, execute it and exit.

   Translated from scan.py
   TODO and semgrep_main.py?
*)

(*****************************************************************************)
(* Logging/Profiling/Debugging *)
(*****************************************************************************)

let setup_logging (conf : Scan_CLI.conf) =
  (* This used to be in Core_CLI.ml, and so should be in CLI.ml but
   * we get a conf object later in osemgrep.
   *)

  (* For osemgrep we use the Logs library instead of the Logger
   * library in pfff. We had a few issues with Logger (which is a small
   * wrapper around the easy_logging library), and we don't really want
   * the logging in semgrep-core to interfere with the proper
   * logging/output we want in osemgrep, so this is a good opportunity
   * to evaluate a new logging library.
   *)
  Logs_helpers.setup_logging conf.logging_level;
  Logs.debug (fun m -> m "Logging setup for semgrep scan");
  Logs.debug (fun m ->
      m "Executed as: %s" (Sys.argv |> Array.to_list |> String.concat " "));

  let config = Core_runner.runner_config_of_conf conf in
  Logs.debug (fun m -> m "Version: %s" config.version);

  (* Easy_logging setup. We should avoid to use Logger in osemgrep/
   * and use Logs instead, but it is still useful to get the semgrep-core
   * logging information at runtime, hence this call.
   *)
  Setup_logging.setup config;
  ()

(* TODO *)
let setup_profiling _conf =
  (* TOADAPT
     if config.debug then Report.mode := MDebug
     else if config.report_time then Report.mode := MTime
     else Report.mode := MNo_info;
  *)
  ()

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

(* python: this used to be done in a _final_raise method from output.py
 * but better separation of concern to do it here.
 *)
let exit_code_of_errors (conf : Scan_CLI.conf)
    (errors : Semgrep_output_v1_t.core_error list) : Exit_code.t =
  match List.rev errors with
  | [] -> Exit_code.ok
  | x :: _ -> (
      (* alt: raise a Semgrep_error that would be catched by CLI_Common
       * wrapper instead of returning an exit code directly? *)
      match () with
      | _ when x.severity = Semgrep_output_v1_t.Error ->
          Cli_json_output.exit_code_of_error_type x.error_type
      | _ when conf.strict ->
          Cli_json_output.exit_code_of_error_type x.error_type
      | _else_ -> Exit_code.ok)

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (conf : Scan_CLI.conf) : Exit_code.t =
  setup_logging conf;
  setup_profiling conf;

  if conf.version then (
    Logs.app (fun m -> m "%s" Version.version);
    (* TOPORT:
       if enable_version_check:
             from semgrep.app.version import version_check
             version_check()
    *)
    Exit_code.ok)
  else
    (* --------------------------------------------------------- *)
    (* Let's go *)
    (* --------------------------------------------------------- *)
    (* TODO: in theory we should have an intermediate module that
     * handle the -e/--lang, or --config, but for now we care
     * only about --config.
     * TODO: in theory we can also pass multiple --config and
     * have a default config.
     *)
    let (rules : Rule.rules), (_errorsTODO : Rule.invalid_rule_error list) =
      Config_resolver.rules_from_dashdash_config conf.config
    in
    (* TODO: there are more ways to specify targets? see target_manager.py *)
    let (targets : Common.filename list), _skipped_targetsTODO =
      Find_target.select_global_targets ~includes:conf.include_
        ~excludes:conf.exclude ~max_target_bytes:conf.max_target_bytes
        ~respect_git_ignore:conf.respect_git_ignore conf.target_roots
    in
    let (res : Core_runner.result) =
      Core_runner.invoke_semgrep_core conf rules targets
    in
    (* outputting the result! in JSON or Text or whatever depending on conf *)
    Output.output_result conf res;
    (* final result for the shell *)
    exit_code_of_errors conf res.core.errors

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let res = Scan_CLI.parse_argv argv in
  (* LATER: this error handling could be factorized probably
   * between the different subcommands at some point
   *)
  match res with
  | Ok conf -> CLI_common.safe_run run conf
  | Error exit_code -> exit_code
