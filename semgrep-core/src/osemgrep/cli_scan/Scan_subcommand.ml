(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-scan command, execute it and exit.

   Translated from scan.py
   TODO and semgrep_main.py?
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (conf : Scan_CLI.conf) : Exit_code.t =
  (* This used to be in Core_CLI.ml, and so should be in CLI.ml but
   * we get a conf object later in osesmgrep.
   *)
  (* --------------------------------------------------------- *)
  (* Setting up debugging/profiling *)
  (* --------------------------------------------------------- *)
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

  (* TOADAPT
     if config.debug then Report.mode := MDebug
     else if config.report_time then Report.mode := MTime
     else Report.mode := MNo_info;
  *)

  (* --------------------------------------------------------- *)
  (* Let's go *)
  (* --------------------------------------------------------- *)
  (* TODO: in theory we should have an intermediate module that
   * handle the -e/--lang, or --config, but for now we care
   * only about --config.
   * TODO: in theory we can also pass multiple --config and
   * have a default config.
   *)
  let rules, _errorsTODO =
    Config_resolver.rules_from_dashdash_config conf.config
  in
  (* TODO: there are more ways to specify targets? see target_manager.py
   *)
  let targets, _skipped_targetsTODO =
    Find_target.select_global_targets ~includes:conf.include_
      ~excludes:conf.exclude ~max_target_bytes:conf.max_target_bytes
      ~respect_git_ignore:conf.respect_git_ignore conf.target_roots
  in
  let (res : Core_runner.result) =
    Core_runner.invoke_semgrep_core conf rules targets
  in
  Output.output_result conf res;
  Exit_code.ok

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
