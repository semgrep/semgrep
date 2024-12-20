open Common
open Fpath_.Operators
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
 *)

(*****************************************************************************)
(* Core error to cli error *)
(*****************************************************************************)
(* LATER: we should get rid of those intermediate Out.core_xxx *)

let core_location_to_error_span (loc : Out.location) : Out.error_span =
  {
    file = loc.path;
    start = loc.start;
    end_ = loc.end_;
    source_hash = None;
    config_start = None;
    config_end = None;
    config_path = None;
    context_start = None;
    context_end = None;
  }

(* Generate error message exposed to user *)
let error_message ~rule_id ~(location : Out.location option)
    ~(error_type : Out.error_type) ~core_message : string =
  let rule_id_str_opt = Option.map Rule_ID.to_string rule_id in
  let error_context =
    match (rule_id_str_opt, error_type) with
    (* For rule errors, the path is a temporary JSON file containing
       the broken rule(s). *)
    | Some id, (RuleParseError | PatternParseError _) -> spf "in rule %s" id
    | ( Some id,
        ( PartialParsing _ | ParseError | OtherParseError | AstBuilderError
        | InvalidYaml | MatchingError | SemgrepMatchFound | TooManyMatches
        | FatalError | Timeout | OutOfMemory | TimeoutDuringInterfile
        | OutOfMemoryDuringInterfile ) ) ->
        let suffix =
          match location with
          | None -> ""
          | Some loc -> spf " on %s" !!(loc.path)
        in
        spf "when running %s%s" id suffix
    | Some id, IncompatibleRule _ -> id
    | Some id, MissingPlugin -> spf "for rule %s" id
    | _ -> (
        match location with
        | None -> ""
        | Some loc -> spf "at line %s:%d" !!(loc.path) loc.start.line)
  in
  (* TODO: this error message can be ugly. It shouldn't contain special
     formatting such as significant newlines and indentation. It should
     also not print the JSON constructor because most of them are not
     English text. Revise this once all the CLI text output is migrated
     to OCaml. *)
  spf "%s %s:\n %s"
    (Error.string_of_error_type error_type)
    error_context core_message

let error_spans ~(error_type : Out.error_type) ~(location : Out.location) =
  match error_type with
  | PatternParseError _yaml_pathTODO ->
      (* TOPORT
         yaml_path = err.error_type.value.value[::-1]
         spans = [dataclasses.replace(..., config_path=yaml_path)]
      *)
      let span =
        (* This code matches the Python code.
           Not sure what it does, frankly. *)
        {
          (core_location_to_error_span location) with
          config_start = Some (Some { line = 0; col = 1; offset = -1 });
          config_end =
            Some
              (Some
                 {
                   line = location.end_.line - location.start.line;
                   col = location.end_.col - location.start.col + 1;
                   offset = -1;
                 });
        }
      in
      Some [ span ]
  | PartialParsing locs -> Some (locs |> List_.map core_location_to_error_span)
  | _else_ -> None

(* # TODO benchmarking code relies on error code value right now
   * # See https://semgrep.dev/docs/cli-usage/ for meaning of codes
*)
let exit_code_of_error_type (error_type : Out.error_type) : Exit_code.t =
  match error_type with
  | ParseError
  | LexicalError
  | PartialParsing _ ->
      Exit_code.invalid_code ~__LOC__
  (* rule errors lead to `missing_config` *)
  | InvalidYaml -> Exit_code.missing_config ~__LOC__
  | OtherParseError
  | AstBuilderError
  | RuleParseError
  | PatternParseError _
  | PatternParseError0
  | MatchingError
  | SemgrepMatchFound
  | TooManyMatches
  | FatalError
  | Timeout
  | OutOfMemory
  | StackOverflow
  | TimeoutDuringInterfile
  | OutOfMemoryDuringInterfile
  (* TODO? really? fatal for SemgrepWarning? *)
  | SemgrepWarning
  | SemgrepError ->
      Exit_code.fatal ~__LOC__
  | InvalidRuleSchemaError -> Exit_code.invalid_pattern ~__LOC__
  | UnknownLanguageError -> Exit_code.invalid_language ~__LOC__
  | IncompatibleRule _
  | IncompatibleRule0
  | MissingPlugin
  | DependencyResolutionError _ ->
      Exit_code.ok ~__LOC__

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
      let exit_code = exit_code_of_error_type error_type in
      let rule_id =
        match error_type with
        (* # Rule id not important for parse errors *)
        | ParseError
        | LexicalError
        | PartialParsing _
        | SemgrepWarning
        | SemgrepError
        | InvalidRuleSchemaError ->
            None
        | OtherParseError
        | AstBuilderError
        | RuleParseError
        | PatternParseError _
        | PatternParseError0
        | InvalidYaml
        | UnknownLanguageError
        | MatchingError
        | SemgrepMatchFound
        | TooManyMatches
        | FatalError
        | Timeout
        | OutOfMemory
        | StackOverflow
        | TimeoutDuringInterfile
        | OutOfMemoryDuringInterfile
        | IncompatibleRule _
        | IncompatibleRule0
        | MissingPlugin
        | DependencyResolutionError _ ->
            rule_id
      in
      let path =
        (* # For rule errors path is a temp file so will just be confusing *)
        match error_type with
        | RuleParseError
        | PatternParseError _
        | PatternParseError0 ->
            None
        | ParseError
        | LexicalError
        | PartialParsing _
        | OtherParseError
        | AstBuilderError
        | InvalidYaml
        | InvalidRuleSchemaError
        | UnknownLanguageError
        | MatchingError
        | SemgrepMatchFound
        | TooManyMatches
        | FatalError
        | Timeout
        | OutOfMemory
        | StackOverflow
        | TimeoutDuringInterfile
        | OutOfMemoryDuringInterfile
        | SemgrepWarning
        | SemgrepError
        | IncompatibleRule _
        | IncompatibleRule0
        | MissingPlugin
        | DependencyResolutionError _ ->
            location |> Option.map (fun (x : Out.location) -> x.path)
      in
      let message =
        Some (error_message ~rule_id ~error_type ~location ~core_message)
      in
      let spans =
        let* location = location in
        error_spans ~error_type ~location
      in
      {
        (* LATER? seems to be either 2 (fatal) or 3 (invalid_code), so maybe
         * better to change the ATD spec and use a variant for cli_error.code
         *)
        code = Exit_code.to_int exit_code;
        level = severity;
        type_ = error_type;
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

let make_fixed_lines fixes_env fix path (start : Out.position)
    (end_ : Out.position) =
  let edit =
    Textedit.
      { path; start = start.offset; end_ = end_.offset; replacement_text = fix }
  in
  Fixed_lines.make_fixed_lines fixes_env edit

let cli_match_of_core_match ~fixed_lines fixed_env (hrules : Rule.hrules)
    (m : Out.core_match) : Out.cli_match =
  match m with
  | {
   check_id = rule_id;
   path;
   start;
   end_;
   extra =
     {
       message;
       severity;
       metadata;
       metavars;
       engine_kind;
       extra_extra;
       validation_state;
       historical_info;
       fix;
       is_ignored;
       dataflow_trace;
       sca_match;
     };
  } ->
      let rule =
        try Hashtbl.find hrules rule_id with
        | Not_found -> raise Impossible
      in
      let rule_message = rule.message in
      let message =
        match message with
        | Some s when not String.(equal empty s) -> s
        | Some _
        | None ->
            rule_message
      in
      let check_id = rule_id in
      let metavars = Some metavars in
      let metadata =
        match metadata with
        | None -> `Assoc []
        | Some json -> json
      in
      (* LATER: this should be a variant in semgrep_output_v1.atd
       * and merged with Constants.rule_severity
       *)
      let severity = severity ||| rule.severity in
      let fixed_lines =
        match (fix, fixed_lines) with
        | None, _
        | _, false ->
            None
        | Some fix, true -> make_fixed_lines fixed_env fix path start end_
      in
      (* Can't use content_of_file_at_range because we want to include the
       * entirety of every line involved in the match, not just the text that
       * matched. *)
      let lines =
        match
          Semgrep_output_utils.lines_of_file_at_range (start, end_) path
        with
        | Ok xs -> xs |> String.concat "\n"
        | Error err ->
            Logs.warn (fun m ->
                m
                  "error on accessing lines of %s; match was with rule %s; \
                   skipping lines field for this match (error was %s)"
                  !!path
                  (Rule_ID.to_string rule_id)
                  err);
            ""
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
            (* fields derived from the rule (and the match) *)
            message;
            severity;
            metadata;
            fix;
            is_ignored = Some is_ignored;
            fingerprint =
              Semgrep_hashing_functions.match_based_id_partial rule rule_id
                metavars !!path;
            sca_info = sca_match;
            fixed_lines;
            dataflow_trace;
            (* It's optional in the CLI output, but not in core match results!*)
            engine_kind = Some engine_kind;
            validation_state;
            historical_info;
            extra_extra;
          };
      }

(* This is the same algorithm for indexing as in pysemgrep. We shouldn't need to update this *)
(* match based ids have an index appended at the end which indicates what
 * # finding of that exact id it is in a file. This is used to dedup findings
 * on the app side.
 * Example:
 * foo.py
bad_function() # bad_function is a finding
bad_function() # 2nd call
 * The above findings will have the exact same match based id, but the index
 * will be different. So the first will be <match_based_id>_0 and the second
 * will be <match_based_id>_1.
 *)
let index_match_based_ids (matches : Out.cli_match list) : Out.cli_match list =
  matches
  (* preserve order *)
  |> List_.mapi (fun i x -> (i, x))
  (* Group by rule and path *)
  (* XXX: can we do with grouping by fingerprint only? *)
  |> Assoc.group_by (fun (_, (x : Out.cli_match)) ->
         (x.path, x.check_id, x.extra.fingerprint))
  (* Sort by start line *)
  |> List_.map (fun (path_and_rule_id, matches) ->
         ( path_and_rule_id,
           List.sort
             (fun (_, (a : Out.cli_match)) (_, (b : Out.cli_match)) ->
               compare a.start.offset b.start.offset)
             matches ))
  (* Index per file *)
  |> List_.map (fun (path_and_rule_id, matches) ->
         let matches =
           List_.mapi
             (fun i (i', (x : Out.cli_match)) ->
               ( i',
                 {
                   x with
                   extra =
                     {
                       x.extra with
                       fingerprint = spf "%s_%d" x.extra.fingerprint i;
                     };
                 } ))
             matches
         in
         (path_and_rule_id, matches))
  (* Flatten *)
  |> List.concat_map snd
  |> List.sort (fun (a, _) (b, _) -> a - b)
  |> List_.map snd

(*****************************************************************************)
(* Gated logged-in fields *)
(*****************************************************************************)

(* coupling: if you modify which fields are gated by ctx.is_logged_in update also
 * https://semgrep.dev/docs/semgrep-appsec-platform/json-and-sarif#json
 * alt: move in Gated_data.ml
 *)
let adjust_fields_cli_outpout_logged_out (x : Out.cli_output) : Out.cli_output =
  (* note: I could use { x with ... } but better to explicitely list the fields
   * here so we see explicitely what we filter and what we do not.
   *)
  let {
    version;
    results;
    errors;
    paths;
    skipped_rules;
    explanations;
    interfile_languages_used = _;
    time;
    rules_by_engine;
    engine_requested;
  } : Out.cli_output =
    x
  in
  let interfile_languages_used = None in
  let results =
    results
    |> List_.map (fun res ->
           let { check_id; extra; path; start; end_ } : Out.cli_match = res in
           let {
             metavars = _;
             message;
             fix;
             fixed_lines;
             metadata;
             severity;
             fingerprint = _;
             lines = _;
             is_ignored = _;
             sca_info;
             dataflow_trace = _;
             engine_kind;
             validation_state;
             historical_info;
             extra_extra;
           } : Out.cli_match_extra =
             extra
           in
           let extra =
             Out.
               {
                 metavars = None;
                 message;
                 fix;
                 fixed_lines;
                 (* TODO? metadata filtering? *)
                 metadata;
                 severity;
                 fingerprint = Gated_data.msg;
                 lines = Gated_data.msg;
                 is_ignored = None;
                 sca_info;
                 dataflow_trace = None;
                 engine_kind;
                 validation_state;
                 historical_info;
                 extra_extra;
               }
           in
           Out.{ check_id; extra; path; start; end_ })
  in
  {
    version;
    results;
    errors;
    paths;
    skipped_rules;
    explanations;
    interfile_languages_used;
    time;
    rules_by_engine;
    engine_requested;
  }

(*****************************************************************************)
(* Cli_output builder *)
(*****************************************************************************)

(* The core/hrules/scanned params are essentially Core_runner.result *)
let cli_output_of_runner_result ~fixed_lines (core : Out.core_output)
    (hrules : Rule.hrules) (scanned : Fpath.t Set_.t) : Out.cli_output =
  match core with
  | {
   version;
   results = matches;
   errors;
   paths =
     {
       skipped;
       (* TODO? should be [] and None given Core_json_output.ml code *)
       scanned = _;
     };
   skipped_rules;
   explanations;
   interfile_languages_used;
   time;
   (* LATER *)
   rules_by_engine = _;
   engine_requested = _;
  } ->
      (* TODO: not sure how it's sorted. Look at rule_match.py keys? *)
      let matches =
        matches
        |> List.sort (fun (a : Out.core_match) (b : Out.core_match) ->
               compare a.check_id b.check_id)
      in
      (* TODO: not sure how it's sorted, but Set_.elements return
       * elements in OCaml compare order (=~ lexicographic for strings)
       * python: scanned=[str(path) for path in sorted(self.all_targets)]
       *)
      let scanned = scanned |> Set_.elements in
      (* Skipping the python intermediate FileTargetingLog for now.
       * We used to have a cli_skipped_target and core_skipped_target type,
       * but now they are merged so this function is the identity.
       * In theory we could remove the details: and rule_id: from it
       * because they used to not be included in the final JSON output
       * (but the info was used in the text output to display skipping
       * information).
       *
       * Still? skipped targets are coming from the FileIgnoreLog which is
       * populated from many places in the code.
       * Still? see _make_failed_to_analyze() in output.py,
       * core_failure_lines_by_file in target_manager.py
       * Still? need to sort
       *)
      let (paths : Out.scanned_and_skipped) = { scanned; skipped } in
      let skipped_rules =
        (* TODO: return skipped_rules with --develop
           if maturity = Develop then
             invalid_rules
           else
        *)
        (* compatibility with pysemgrep *)
        ignore skipped_rules;
        []
      in
      let fixed_env = Fixed_lines.mk_env () in
      {
        version = Some version;
        (* Skipping the python intermediate RuleMatchMap for now *)
        results =
          matches
          |> List_.map (cli_match_of_core_match ~fixed_lines fixed_env hrules)
          |> Semgrep_output_utils.sort_cli_matches;
        errors = errors |> List_.map cli_error_of_core_error;
        paths;
        skipped_rules;
        explanations;
        interfile_languages_used;
        time;
        (* LATER *)
        rules_by_engine = None;
        engine_requested = None;
      }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let json_output (ctx : Out.format_context) (cli_output : Out.cli_output) =
  let cli_output =
    if ctx.is_logged_in then cli_output
    else adjust_fields_cli_outpout_logged_out cli_output
  in
  Out.string_of_cli_output cli_output
