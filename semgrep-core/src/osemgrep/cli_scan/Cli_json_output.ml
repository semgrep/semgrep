open Common
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Convert results coming from the core-runner (semgrep-core JSON output)
 * to the official Semgrep CLI JSON output.
 *
 * I'm skipping lots of Python code and lots of intermediate modules for now
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

(* LATER: use Metavariable.bindings directly ! *)
type metavars = (string * Out.metavar_value) list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let string_of_severity (severity : Rule.severity) : string =
  match severity with
  | Error -> "ERROR"
  | Warning -> "WARNING"
  | Info -> "INFO"
  | Experiment -> "EXPERIMENT"
  | Inventory -> "INVENTORY"

let config_prefix_of_conf (conf : Scan_CLI.conf) : string =
  (* TODO: what if it's a registry rule?
   * call Semgrep_dashdash_config.config_kind_of_config_str
   *)
  let path = conf.config in
  (*  need to prefix with the dotted path of the config file *)
  let dir = Filename.dirname path in
  Str.global_replace (Str.regexp "/") "." dir ^ "."

(* Return the list of lines for a start/end range. Note that
 * we take the whole line. Note also that each line does not contain
 * a trailing "\n" so you may need to String.concat "\n" if you join them.
 *
 * TODO: at some point we should take a Parse_info range, not Out.position
 *  (but in that case don't forget to use Parse_info.get_token_end_info end_)
 * TODO: could be moved to another helper module.
 *
 * Should we use a faster implementation, using a cache
 * to avoid rereading the same file again and again? probably fast
 * enough like this thanks to OS buffer cache.
 *
 * python: # 'lines' already contains '\n' at the end of each line
 *   lines="".join(rule_match.lines).rstrip(),
 *)
let lines_of_file (range : Out.position * Out.position) (file : filename) :
    string list =
  let start, end_ = range in
  Matching_report.lines_of_file (start.line, end_.line) file
  [@@profiling]

(* Returns the text between the positions; start inclusive, end exclusive.
 * TODO: same than above, ideally would take a Parse_info range
 * TOPORT: It is recommended to open the fd with `open(path, errors="replace")
 * to ignore non-utf8 bytes.
 * See https://stackoverflow.com/a/56441652.
 *)
let content_of_file (range : Out.position * Out.position) (file : filename) :
    string =
  let start, end_ = range in
  let str = Common.read_file file in
  String.sub str start.offset (end_.offset - start.offset)
  [@@profiling]

(* Substitute the metavariables mentioned in a message to their
 * matched content.
 *
 * We could either:
 *  (1) go through all the metavars and textually substitute them in the text
 *  (2) go through the text and find each metavariable regexp occurence
 *    and replace them with their content
 * python: the original code did (1) so we're doing the same for now,
 * however (2) seems more logical to me and wasting less CPUs since
 * you only substitute metavars that are actually mentioned in the message.
 *)
let interpolate_metavars (text : string) (metavars : metavars) (file : filename)
    : string =
  (* sort by metavariable length to avoid name collisions
   * (eg. $X2 must be handled before $X)
   *)
  let mvars =
    metavars
    |> List.sort (fun (a, _) (b, _) ->
           compare (String.length b) (String.length a))
  in
  mvars
  |> List.fold_left
       (fun text (mvar, mval) ->
         (* necessary typing to help the type check disambiguate fields,
          * because of the use of multiple fields with the same
          * name in semgrep_output_v1.atd *)
         let (v : Out.metavar_value) = mval in
         let content = lazy (content_of_file (v.start, v.end_) file) in
         text
         (* first value($X), and then $X *)
         |> Str.global_substitute
              (Str.regexp_string (spf "value(%s)" mvar))
              (fun _whole_str ->
                match v.propagated_value with
                | Some x ->
                    x.svalue_abstract_content (* default to the matched value *)
                | None -> Lazy.force content)
         |> Str.global_substitute (Str.regexp_string mvar) (fun _whole_str ->
                Lazy.force content))
       text

(*****************************************************************************)
(* Core error to cli error *)
(*****************************************************************************)
(* LATER: we should get rid of those intermediate Out.core_xxx *)

let cli_error_of_core_error (_xTODO : Out.core_error) : Out.cli_error =
  {
    code = 1;
    level = "TODO1";
    type_ = "TODO2";
    rule_id = None;
    message = None;
    path = None;
    long_msg = None;
    short_msg = None;
    spans = None;
    help = None;
  }

(*****************************************************************************)
(* Core match to cli match *)
(*****************************************************************************)
(* LATER: we should get rid of those intermediate Out.core_xxx *)

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
        (* message where the metavars have been interpolated *)
        | Some s -> interpolate_metavars s metavars path
        | None -> ""
      in
      let fix =
        (* TOPORT: debug logging which indicates the source of the fix *)
        match (x.extra.rendered_fix, rule.fix) with
        | Some fix, _ -> Some fix
        | None, Some fix -> Some (interpolate_metavars fix metavars path)
        | None, None -> None
      in
      (*  need to prefix with the dotted path of the config file *)
      let check_id = env.config_prefix ^ rule_id in
      let metavars = Some metavars in
      (* LATER: this should be a variant in semgrep_output_v1.atd
       * and merged with Constants.rule_severity
       *)
      let severity = string_of_severity rule.severity in
      let metadata =
        match rule.metadata with
        | None -> `Assoc []
        | Some json -> JSON.to_yojson json
      in
      let lines = lines_of_file (start, end_) path |> String.concat "\n" in
      {
        check_id;
        path;
        start;
        end_;
        extra =
          {
            metavars;
            lines;
            (* fields derived from the rule *)
            message;
            severity;
            metadata;
            fix;
            (* TODO: other fields derived from the rule *)
            fix_regex = None;
            (* TODO: extra fields *)
            is_ignored = Some false;
            (* LATER *)
            (* TODO: rule_match.match_based_id *)
            fingerprint = "0x42";
            sca_info = None;
            fixed_lines = None;
            dataflow_trace = None;
          };
      }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

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
      (* TODO: not sure how it's sorted. Look at rule_match.py keys? *)
      let matches =
        matches
        |> List.sort (fun (a : Out.core_match) (b : Out.core_match) ->
               compare a.rule_id b.rule_id)
      in
      (* TODO: not sure how it's sorted, but Set_.elements return
       * elements in OCaml compare order (=~ lexicographic for strings)
       *)
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
