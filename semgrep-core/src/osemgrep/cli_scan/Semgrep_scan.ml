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
(* Helpers *)
(*****************************************************************************)

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

let cli_match_of_core_match (hrules : Rule.hrules) (x : Out.core_match) :
    Out.cli_match =
  match x with
  | {
   rule_id;
   location;
   extra =
     { message; metavars; (* LATER *)
                          dataflow_trace = _; rendered_fix = _ };
  } ->
      let rule =
        try Hashtbl.find hrules rule_id with
        | Not_found -> raise Impossible
      in
      let path = location.path in
      let start = location.start in
      let end_ = location.end_ in
      let message =
        match message with
        (* TODO: message where the metavars have been interpolated *)
        | Some s -> s
        | None -> ""
      in
      (* TODO: need to prefix with the dotted path of the config file,
       * e.g., semgrep-core.tests.osemgrep... if the argument
       * was --config semgrep-core/tests/osemgrep.yml
       *)
      let check_id = rule_id in
      let metavars = Some metavars in
      (* LATER: this should be a variant in semgrep_output_v0.atd
       * and merged with Constants.rule_severity
       *)
      let severity =
        match rule.severity with
        | Error -> "ERROR"
        | Warning -> "WARNING"
        | Info -> "INFO"
        | Experiment -> "EXPERIMENT"
        | Inventory -> "INVENTORY"
      in
      let metadata =
        match rule.metadata with
        | None -> `Assoc []
        | Some json -> JSON.to_yojson json
      in
      {
        check_id;
        path;
        start;
        end_;
        extra =
          {
            metavars;
            (* TODO *)
            lines = "TODO";
            message;
            (* fields derived from the rule *)
            severity;
            metadata;
            (* TODO: other fields derived from the rule *)
            fix = None;
            fix_regex = None;
            (* TODO: extra fields *)
            is_ignored = Some false;
            (* LATER *)
            fingerprint = "TODO";
            sca_info = None;
            fixed_lines = None;
            dataflow_trace = None;
          };
      }

let cli_output_of_core_results (res : Core_runner.result) : Out.cli_output =
  match res.core with
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
      (* TODO: not sure how it's sorted *)
      let matches =
        matches
        |> List.sort (fun (a : Out.core_match) (b : Out.core_match) ->
               compare a.rule_id b.rule_id)
      in
      {
        version = Some Version.version;
        results = matches |> Common.map (cli_match_of_core_match res.hrules);
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
  let (res : Core_runner.result) = Core_runner.invoke_semgrep_core conf in
  let (cli_output : Out.cli_output) = cli_output_of_core_results res in

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
