open Common
open Fpath_.Operators
module OutJ = Semgrep_output_v1_j

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

let core_location_to_error_span (loc : OutJ.location) : OutJ.error_span =
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
let error_message ~rule_id ~(location : OutJ.location)
    ~(error_type : OutJ.error_type) ~core_message : string =
  let path = location.path in
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
        spf "when running %s on %s" id !!path
    | Some id, IncompatibleRule _ -> id
    | Some id, MissingPlugin -> spf "for rule %s" id
    | _ -> spf "at line %s:%d" !!path location.start.line
  in
  spf "%s %s:\n %s"
    (Error.string_of_error_type error_type)
    error_context core_message

let error_spans ~(error_type : OutJ.error_type) ~(location : OutJ.location) =
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
let exit_code_of_error_type (error_type : OutJ.error_type) : Exit_code.t =
  match error_type with
  | ParseError
  | LexicalError
  | PartialParsing _ ->
      Exit_code.invalid_code ~__LOC__
  | OtherParseError
  | AstBuilderError
  | RuleParseError
  | PatternParseError _
  | PatternParseError0
  | InvalidYaml
  | MatchingError
  | SemgrepMatchFound
  | TooManyMatches
  | FatalError
  | Timeout
  | OutOfMemory
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
  | MissingPlugin ->
      Exit_code.ok ~__LOC__

(* Skipping the intermediate python SemgrepCoreError for now.
 * TODO: should we return an Error.Semgrep_core_error instead? like we
 * do in python? and then generate an Out.cli_error out of it?
 *)
let cli_error_of_core_error (x : OutJ.core_error) : OutJ.cli_error =
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
        | TimeoutDuringInterfile
        | OutOfMemoryDuringInterfile
        | IncompatibleRule _
        | IncompatibleRule0
        | MissingPlugin ->
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
        | TimeoutDuringInterfile
        | OutOfMemoryDuringInterfile
        | SemgrepWarning
        | SemgrepError
        | IncompatibleRule _
        | IncompatibleRule0
        | MissingPlugin ->
            Some location.path
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

let make_fixed_lines ?applied_fixes lines fix path (start : OutJ.position)
    (end_ : OutJ.position) =
  let fix_overlaps, add_fix =
    match applied_fixes with
    | None -> (false, fun () -> ())
    | Some table ->
        let v =
          match Hashtbl.find_opt table !!path with
          | Some xs -> xs
          | None -> []
        in
        ( List.exists
            (fun (st, en) -> st <= start.offset && en >= start.offset)
            v,
          fun () ->
            Hashtbl.replace table !!path ((start.offset, end_.offset) :: v) )
  in
  if String.equal fix "" then None
  else if fix_overlaps then None
  else
    match (lines, List.rev lines) with
    | line :: _, last_line :: _ ->
        let first_line_part = Str.first_chars line (start.col - 1)
        and last_line_part = Str.string_after last_line (end_.col - 1) in
        add_fix ();
        Some
          (String.split_on_char '\n' (first_line_part ^ fix ^ last_line_part))
    | [], _
    | _, [] ->
        None

let cli_match_of_core_match ~dryrun ?applied_fixes (hrules : Rule.hrules)
    (m : OutJ.core_match) : OutJ.cli_match =
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
      (* LATER: this should be a variant in semgrep_output_v1.atd
       * and merged with Constants.rule_severity
       *)
      let severity = severity ||| rule.severity in
      let metadata =
        match rule.metadata with
        | None -> `Assoc []
        | Some json -> (
            JSON.to_yojson json |> fun rule_metadata ->
            match metadata with
            | Some metadata -> JSON.update rule_metadata metadata
            | None -> rule_metadata)
      in
      (* TODO? at this point why not using content_of_file_at_range since
       * we concatenate the lines after? *)
      let lines =
        Semgrep_output_utils.lines_of_file_at_range (start, end_) path
      in
      let fixed_lines =
        match (fix, dryrun) with
        | None, _
        | _, false ->
            None
        | Some fix, true ->
            make_fixed_lines ?applied_fixes lines fix path start end_
      in
      let lines = lines |> String.concat "\n" in
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
            (* TODO: extra fields *)
            fingerprint =
              Semgrep_hashing_functions.match_based_id_partial rule rule_id
                metavars !!path;
            sca_info = None;
            fixed_lines;
            dataflow_trace;
            (* It's optional in the CLI output, but not in the core match results!
             *)
            engine_kind = Some engine_kind;
            validation_state;
            historical_info;
            extra_extra;
          };
      }

(*
 # Sort results so as to guarantee the same results across different
 # runs. Results may arrive in a different order due to parallelism
 # (-j option).
*)
let dedup_and_sort (xs : OutJ.cli_match list) : OutJ.cli_match list =
  let seen = Hashtbl.create 101 in
  xs
  |> List.filter (fun x ->
         let key = Semgrep_hashing_functions.cli_unique_key x in
         if Hashtbl.mem seen key then false
         else (
           Hashtbl.replace seen key true;
           true))
  |> Semgrep_output_utils.sort_cli_matches

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
let index_match_based_ids (matches : OutJ.cli_match list) : OutJ.cli_match list
    =
  matches
  (* preserve order *)
  |> List_.mapi (fun i x -> (i, x))
  (* Group by rule and path *)
  (* XXX: can we do with grouping by fingerprint only? *)
  |> Assoc.group_by (fun (_, (x : OutJ.cli_match)) ->
         (x.path, x.check_id, x.extra.fingerprint))
  (* Sort by start line *)
  |> List_.map (fun (path_and_rule_id, matches) ->
         ( path_and_rule_id,
           List.sort
             (fun (_, (a : OutJ.cli_match)) (_, (b : OutJ.cli_match)) ->
               compare a.start.offset b.start.offset)
             matches ))
  (* Index per file *)
  |> List_.map (fun (path_and_rule_id, matches) ->
         let matches =
           List_.mapi
             (fun i (i', (x : OutJ.cli_match)) ->
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
(* Entry point *)
(*****************************************************************************)

(* The 3 parameters are mostly Core_runner.result but we don't want
 * to depend on cli_scan/ from reporting/ here, hence the duplication.
 * alt: we could move Core_runner.result type in core/
 *)
let cli_output_of_core_results ~dryrun ~logging_level (core : OutJ.core_output)
    (hrules : Rule.hrules) (scanned : Fpath.t Set_.t) : OutJ.cli_output =
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
   (* LATER *)
   time = _;
   rules_by_engine = _;
   engine_requested = _;
  } ->
      (* TODO: not sure how it's sorted. Look at rule_match.py keys? *)
      let matches =
        matches
        |> List.sort (fun (a : OutJ.core_match) (b : OutJ.core_match) ->
               compare a.check_id b.check_id)
      in
      (* TODO: not sure how it's sorted, but Set_.elements return
       * elements in OCaml compare order (=~ lexicographic for strings)
       * python: scanned=[str(path) for path in sorted(self.all_targets)]
       *)
      let scanned = scanned |> Set_.elements in
      let (paths : OutJ.scanned_and_skipped) =
        match logging_level with
        | Some (Logs.Info | Logs.Debug) ->
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
            { scanned; skipped }
        | _else_ -> { scanned; skipped = None }
      in
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
      let applied_fixes = Hashtbl.create 13 in
      {
        version;
        (* Skipping the python intermediate RuleMatchMap for now.
         * TODO: handle the rule_match.cli_unique_key to dedup matches
         *)
        results =
          matches
          |> List_.map (cli_match_of_core_match ~dryrun ~applied_fixes hrules)
          |> dedup_and_sort;
        errors = errors |> List_.map cli_error_of_core_error;
        paths;
        skipped_rules;
        explanations;
        interfile_languages_used;
        (* LATER *)
        time = None;
        rules_by_engine = None;
        engine_requested = None;
      }
