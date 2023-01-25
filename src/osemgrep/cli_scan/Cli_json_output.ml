open Common
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Convert results coming from Core_runner (semgrep-core JSON output)
 * to the formally specified Semgrep CLI JSON output.
 *
 * I'm skipping lots of Python code and lots of intermediate modules for now
 * and just go directly from the Core_runner results to the final Cli_output.
 * In the Python codebase it goes through many intermediate data-structures
 * (e.g., RuleMatchMap, SemgrepCoreError, FileTargetingLog, ProfilingData)
 * and many modules:
 *  - scan.py
 *  - semgrep_main.py
 *  - core_runner.py
 *  - core_output.py
 *  - error.py
 *  - output.py
 *  - formatter/base.py
 *  - formatter/json.py
 *
 *  LATER? like for Output.ml it would be nice to move this file to
 *  osemgrep/reporting/.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* environment to pass to the JSON cli_output generator *)
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
(* File content accessors *)
(*****************************************************************************)

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
let contents_of_file (range : Out.position * Out.position) (file : filename) :
    string =
  let start, end_ = range in
  let str = Common.read_file file in
  String.sub str start.offset (end_.offset - start.offset)
  [@@profiling]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* TODO: probably want to use Config_resolver.rules_and_origin to
 * get the prefix
 *)
let config_prefix_of_rules_source (src : Rule_fetching.rules_source) : string =
  (* TODO: what if it's a registry rule?
   * call Semgrep_dashdash_config.config_kind_of_config_str
   *)
  match src with
  | Rule_fetching.Configs (path :: _TODO) ->
      (*  need to prefix with the dotted path of the config file *)
      let dir = Filename.dirname path in
      Str.global_replace (Str.regexp "/") "." dir ^ "."
  | _else_ -> ""

(*****************************************************************************)
(* Core error to cli error *)
(*****************************************************************************)
(* LATER: we should get rid of those intermediate Out.core_xxx *)

let core_location_to_error_span (loc : Out.location) : Out.error_span =
  {
    file = loc.path;
    start = { line = loc.start.line; col = loc.start.col };
    end_ = { line = loc.end_.line; col = loc.end_.col };
    source_hash = None;
    config_start = None;
    config_end = None;
    config_path = None;
    context_start = None;
    context_end = None;
  }

(* LATER: move to Severity.ml, and use Severity.rule_severity instead? *)
let string_of_severity (severity : Rule.severity) : string =
  match severity with
  | Error -> "ERROR"
  | Warning -> "WARNING"
  | Info -> "INFO"
  | Experiment -> "EXPERIMENT"
  | Inventory -> "INVENTORY"

(* LATER: move also to Severity.ml and reuse types there *)
let level_of_severity (severity : Out.core_severity) : Severity.basic_severity =
  match severity with
  | Error -> `Error
  | Warning -> `Warning

let error_type_string (error_type : Out.core_error_kind) : string =
  match error_type with
  (* # convert to the same string of core.ParseError for now *)
  | PartialParsing _ -> "Syntax error"
  | PatternParseError _ -> "Pattern parse error"
  (* # All the other cases don't have arguments in Semgrep_output_v1.atd
   * # and have some <json name="..."> annotations to generate the right string
   * python: str(type_.to_json())
   * but safer to just enumerate and write the boilerplate in OCaml
   *)
  | LexicalError -> "Lexical error"
  | ParseError -> "Syntax error"
  | SpecifiedParseError -> "Other syntax error"
  | AstBuilderError -> "AST builder error"
  | RuleParseError -> "Rule parse error"
  | InvalidYaml -> "Invalid YAML"
  | MatchingError -> "Internal matching error"
  | SemgrepMatchFound -> "Semgrep match found"
  | TooManyMatches -> "Too many matches"
  | FatalError -> "Fatal error"
  | Timeout -> "Timeout"
  | OutOfMemory -> "Out of memory"
  | TimeoutDuringPreprocessing -> "Timeout during preprocessing"
  | OutOfMemoryDuringPreprocessing -> "Out of memory during preprocessing"

(* Generate error message exposed to user *)
let error_message ~rule_id ~(location : Out.location)
    ~(error_type : Out.core_error_kind) ~core_message : string =
  let path = location.path in
  let error_context =
    match (rule_id, error_type) with
    (* # For rule errors, path is a temp file so will just be confusing *)
    | Some id, (RuleParseError | PatternParseError _) -> spf "in rule %s" id
    | Some id, _else_ -> spf "when running %s on %s" id path
    | _else_ -> spf "at line %s:%d" path location.start.line
  in
  spf "%s %s:\n %s" (error_type_string error_type) error_context core_message

let error_spans ~(error_type : Out.core_error_kind) ~(location : Out.location) =
  match error_type with
  | PatternParseError _yaml_pathTODO ->
      (* TOPORT
         yaml_path = err.error_type.value.value[::-1]
         spans = [dataclasses.replace(..., config_path=yaml_path)]
      *)
      let span =
        {
          (core_location_to_error_span location) with
          config_start = Some (Some { line = 0; col = 1 });
          config_end =
            Some
              (Some
                 {
                   line = location.end_.line - location.start.line;
                   col = location.end_.col - location.start.col + 1;
                 });
        }
      in
      Some [ span ]
  | PartialParsing locs -> Some (locs |> Common.map core_location_to_error_span)
  | _else_ -> None

(* # TODO benchmarking code relies on error code value right now
   * # See https://semgrep.dev/docs/cli-usage/ for meaning of codes
*)
let exit_code_of_error_type (error_type : Out.core_error_kind) : Exit_code.t =
  match error_type with
  | ParseError
  | LexicalError
  | PartialParsing _ ->
      Exit_code.invalid_code
  | _else_ -> Exit_code.fatal

(* Skipping the intermediate python SemgrepCoreError for now.
 * TODO: should we return an Error.Semgrep_core_error instead? like we
 * do in python? and then generate an Out.cli_error out of it?
 *)
let cli_error_of_core_error (x : Out.core_error) : Out.cli_error =
  match x with
  | {
   error_type;
   severity;
   location;
   message = core_message;
   rule_id;
   (* LATER *) details = _;
  } ->
      let level = level_of_severity severity in
      let exit_code = exit_code_of_error_type error_type in
      let rule_id =
        match error_type with
        (* # Rule id not important for parse errors *)
        | ParseError
        | LexicalError
        | PartialParsing _ ->
            None
        | _else_ -> rule_id
      in
      let path =
        (* # For rule errors path is a temp file so will just be confusing *)
        match error_type with
        | RuleParseError
        | PatternParseError _ ->
            None
        | _else_ -> Some location.path
      in
      let message =
        Some (error_message ~rule_id ~error_type ~location ~core_message)
      in
      let spans = error_spans ~error_type ~location in
      {
        (* LATER? seems to be either 2 (fatal) or 3 (invalid_code), so maybe
         * better to change the ATD spec and use a variant for cli_error.code
         *)
        code = Exit_code.to_int exit_code;
        (* LATER: should use a variant too *)
        level = Severity.string_of_basic_severity level;
        (* LATER: type_ should be a proper variant instead of a string *)
        type_ = error_type_string error_type;
        rule_id;
        path;
        message;
        spans;
        (* LATER *)
        long_msg = None;
        short_msg = None;
        help = None;
      }

(*****************************************************************************)
(* Core match to cli match *)
(*****************************************************************************)
(* LATER: we should get rid of those intermediate Out.core_xxx *)

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
         let content = lazy (contents_of_file (v.start, v.end_) file) in
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

let cli_match_of_core_match (env : env) (x : Out.core_match) : Out.cli_match =
  match x with
  | {
   rule_id;
   location;
   extra =
     {
       message;
       metavars;
       rendered_fix;
       (* LATER *)
       dataflow_trace = _;
       engine_kind;
     };
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
        match (rendered_fix, rule.fix) with
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
            (* fields derived from the rule (and the match) *)
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
            engine_kind;
          };
      }

(*
 # Sort results so as to guarantee the same results across different
 # runs. Results may arrive in a different order due to parallelism
 # (-j option).
 TOPORT: return {rule: sorted(matches) for rule, matches in findings.items()}
*)
let dedup_and_sort (xs : Out.cli_match list) : Out.cli_match list =
  let seen = Hashtbl.create 101 in
  xs
  |> List.filter (fun x ->
         if Hashtbl.mem seen x then false
         else
           (* TOPORT: use rule_match.cli_unique_key to dedup (not the whole x) *)
           let key = x in
           Hashtbl.replace seen key true;
           true)

(*****************************************************************************)
(* Skipped target *)
(*****************************************************************************)

let cli_skipped_target_of_skipped_target (_x : Out.skipped_target) :
    Out.cli_skipped_target =
  failwith "TODO:cli_skipped_target_of_skipped_target"

(* skipping the python intermediate FileTargetingLog for now *)
let cli_skipped_targets_opt ~(errors : Out.core_error list)
    ~(skipped_targets : Out.skipped_target list option) :
    Out.cli_skipped_target list option =
  (* TODO: skipped targets are coming from the FileIgnoreLog which is
   * populated from many places in the code.
   *)

  (* TODO: see _make_failed_to_analyze() in output.py,
   * core_failure_lines_by_file in target_manager.py
   *)
  let skipped_because_of_core_errors : Out.cli_skipped_target list =
    errors
    |> Common.map (fun (err : Out.core_error) ->
           {
             Out.path = err.location.path;
             reason = "analysis_failed_parser_or_internal_error";
           })
  in
  let core_skipped =
    match skipped_targets with
    | None -> []
    | Some xs -> xs |> Common.map cli_skipped_target_of_skipped_target
  in
  (* TODO: need to sort *)
  let final = skipped_because_of_core_errors @ core_skipped in
  if null final then None else Some final

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let cli_output_of_core_results ~logging_level ~rules_source
    (res : Core_runner.result) : Out.cli_output =
  match res.core with
  | {
   matches;
   errors;
   skipped_targets;
   (* LATER *)
   skipped_rules = _;
   explanations = _;
   stats = _;
   time = _;
  } ->
      let env =
        {
          hrules = res.hrules;
          config_prefix = config_prefix_of_rules_source rules_source;
        }
      in
      (* TODO: not sure how it's sorted. Look at rule_match.py keys? *)
      let matches =
        matches
        |> List.sort (fun (a : Out.core_match) (b : Out.core_match) ->
               compare a.rule_id b.rule_id)
      in
      (* TODO: not sure how it's sorted, but Set_.elements return
       * elements in OCaml compare order (=~ lexicographic for strings)
       * python: scanned=[str(path) for path in sorted(self.all_targets)]
       *)
      let scanned = res.scanned |> Set_.elements in
      let (paths : Out.cli_paths) =
        match logging_level with
        | Some Logs.Info ->
            let skipped = cli_skipped_targets_opt ~errors ~skipped_targets in
            { scanned; _comment = None; skipped }
        | _else_ ->
            {
              scanned;
              _comment = Some "<add --verbose for a list of skipped paths>";
              skipped = None;
            }
      in
      {
        version = Some Version.version;
        (* Skipping the python intermediate RuleMatchMap for now.
         * TODO: handle the rule_match.cli_unique_key to dedup matches
         *)
        results =
          matches |> Common.map (cli_match_of_core_match env) |> dedup_and_sort;
        errors = errors |> Common.map cli_error_of_core_error;
        paths;
        (* LATER *)
        time = None;
        explanations = None;
      }
