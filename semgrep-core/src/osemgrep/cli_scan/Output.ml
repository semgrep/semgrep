open Common
module Out = Semgrep_output_v0_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep scan' output.

   Partially translated from output.py

   For now only the JSON output is supported.
   TODO? move most of the content of this file to Output_JSON.ml?
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* environment to pass to the cli_output generator *)
type env = {
  hrules : Rule.hrules;
  (* string to prefix all rule_id with
   * (e.g., "semgrep-core.tests." if the --config argument
    * was semgrep-core/tests/osemgrep.yml)
   *)
  config_prefix : string;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let config_prefix_of_conf (conf : Scan_CLI.conf) : string =
  (* TODO: what if it's a registry rule? *)
  let path = conf.config in
  (*  need to prefix with the dotted path of the config file *)
  let dir = Filename.dirname path in
  dir ^ "."

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

let cli_match_of_core_match (env : env) (x : Out.core_match) : Out.cli_match =
  match x with
  | {
   rule_id;
   location;
   extra =
     { message; metavars; (* LATER *)
                          dataflow_trace = _; rendered_fix = _ };
  } ->
      let rule =
        try Hashtbl.find env.hrules rule_id with
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
      (*  need to prefix with the dotted path of the config file *)
      let check_id = env.config_prefix ^ rule_id in
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
      (* TODO: we should use a faster implementation, using a cache
       * to avoid rereading the same file again and again
       *)
      let lines =
        Matching_report.lines_of_file (start.line, end_.line) path
        |> String.concat "\n"
      in
      {
        check_id;
        path;
        start;
        end_;
        extra =
          {
            metavars;
            lines;
            (* TODO *)
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

let cli_output_of_core_results (conf : Scan_CLI.conf) (res : Core_runner.result)
    : Out.cli_output =
  match res.core with
  | {
   matches;
   errors;
   (* LATER *)
   skipped_targets = _;
   skipped_rules = _;
   explanations = _;
   stats = _;
   time = _;
  } ->
      let env =
        { hrules = res.hrules; config_prefix = config_prefix_of_conf conf }
      in
      (* TODO: not sure how it's sorted *)
      let matches =
        matches
        |> List.sort (fun (a : Out.core_match) (b : Out.core_match) ->
               compare a.rule_id b.rule_id)
      in
      (* TODO: not sure how it's sorted *)
      let scanned = res.scanned |> Set_.elements in
      {
        version = Some Version.version;
        results = matches |> Common.map (cli_match_of_core_match env);
        paths =
          {
            scanned;
            _comment = Some "<add --verbose for a list of skipped paths>";
            (* TOPORT *)
            skipped = None;
          };
        errors = errors |> Common.map cli_error_of_core_error;
        (* LATER *)
        time = None;
        explanations = None;
      }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let output_result (conf : Scan_CLI.conf) (res : Core_runner.result) : unit =
  let (cli_output : Out.cli_output) = cli_output_of_core_results conf res in
  (* TODO: if conf.output_format = Json *)
  let s = Out.string_of_cli_output cli_output in
  pr s;
  ()
