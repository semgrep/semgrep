open Common
module Out = Semgrep_output_v0_j

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-scan command, execute it and exit.

   Translated from scan.py
   TODO and semgrep_main.py?
*)

(*****************************************************************************)
(* Core output to Cli output *)
(*****************************************************************************)
(* I'm skipping lots of Python code and lots of intermediate modules for now
 * and just go directly to the final Cli_output.
 * In the Python codebase it goes through intermediate data-structures
 * (e.g., RuleMatchMap, ProfilingData) and many modules:
 *  - scan.py
 *  - semgrep_main.py
 *  - core_runner.py
 *  - core_output.py
 *  - output.py
 *  - formatter/base.py
 *  - formatter/json.py
 *)

let cli_error_of_core_error (_x : Out.core_error) : Out.cli_error =
  failwith "TODO: cli_error_of_core_error"

let cli_match_of_core_match (x : Out.core_match) : Out.cli_match =
  match x with
  | {
   rule_id;
   (* TODO *)
   location;
   extra =
     {
       message = _;
       metavars = _;
       (* LATER *)
       dataflow_trace = _;
       rendered_fix = _;
     };
  } ->
      let path = location.path in
      let start = location.start in
      let end_ = location.end_ in
      {
        check_id = rule_id;
        path;
        start;
        end_;
        extra =
          {
            (* TODO *)
            metavars = None;
            fingerprint = "TODO";
            lines = "TODO";
            (* TODO: fields derived from the rule *)
            message = "";
            metadata = `Null;
            severity = "TODO";
            fix = None;
            fix_regex = None;
            (* TODO: extra fields *)
            is_ignored = None;
            (* LATER *)
            sca_info = None;
            fixed_lines = None;
            dataflow_trace = None;
          };
      }

let cli_output_of_core_match_results (res : Out.core_match_results) :
    Out.cli_output =
  match res with
  | {
   matches;
   (* TODO *)
   errors;
   (* LATER *)
   skipped_targets = _;
   skipped_rules = _;
   explanations = _;
   stats = _;
   time = _;
  } ->
      {
        version = Some Version.version;
        results = matches |> Common.map cli_match_of_core_match;
        (* TODO *)
        paths =
          {
            scanned = [];
            _comment = Some "<add --verbose for a list of skipped paths>";
            skipped = None;
          };
        errors = errors |> Common.map cli_error_of_core_error;
        (* LATER *)
        time = None;
        explanations = None;
      }

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

  (* TOADAPT
     if config.debug then Report.mode := MDebug
     else if config.report_time then Report.mode := MTime
     else Report.mode := MNo_info;
  *)
  let config = Core_runner.runner_config_of_conf conf in
  Setup_logging.setup config;

  logger#info "Executed as: %s" (Sys.argv |> Array.to_list |> String.concat " ");
  logger#info "Version: %s" config.version;

  (* !!!TODO!!! use the result!! see semgrep_main.py *)
  let (res : Out.core_match_results) = Core_runner.invoke_semgrep_core conf in
  let (cli_output : Out.cli_output) = cli_output_of_core_match_results res in

  (* TODO: if conf.output_format = Json *)
  let s = Out.string_of_cli_output cli_output in
  pr s;

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
