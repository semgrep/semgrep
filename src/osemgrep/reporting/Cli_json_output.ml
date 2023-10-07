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
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* LATER: use Metavariable.bindings directly ! *)
type metavars = (string * Out.metavar_value) list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

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
 *
 * TODO: expose this function so it can be used in language_server
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
         let content =
           lazy
             (Semgrep_output_utils.content_of_file_at_range (v.start, v.end_)
                (Fpath.v file))
         in
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

(* TODO: expose this function so it can be used in language_server *)
let render_fix (hrules : Rule.hrules) (x : Out.core_match) : string option =
  match x with
  | { check_id = rule_id; path; extra = { metavars; rendered_fix; _ }; _ } -> (
      let rule =
        try Hashtbl.find hrules (Rule_ID.of_string rule_id) with
        | Not_found -> raise Impossible
      in
      (* TOPORT: debug logging which indicates the source of the fix *)
      match (rendered_fix, rule.fix) with
      | Some fix, _ -> Some fix
      | None, Some fix -> Some (interpolate_metavars fix metavars path)
      | None, None -> None)

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
  | TimeoutDuringInterfile -> "Timeout during interfile analysis"
  | OutOfMemoryDuringInterfile -> "OOM during interfile analysis"
  | IncompatibleRule _ -> "Incompatible rule"
  | MissingPlugin -> "Missing plugin"

(* Generate error message exposed to user *)
let error_message ~rule_id ~(location : Out.location)
    ~(error_type : Out.core_error_kind) ~core_message : string =
  let path = location.path in
  let error_context =
    match (rule_id, error_type) with
    (* For rule errors, the path is a temporary JSON file containing
       the broken rule(s). *)
    | Some id, (RuleParseError | PatternParseError _) -> spf "in rule %s" id
    | ( Some id,
        ( PartialParsing _ | ParseError | SpecifiedParseError | AstBuilderError
        | InvalidYaml | MatchingError | SemgrepMatchFound | TooManyMatches
        | FatalError | Timeout | OutOfMemory | TimeoutDuringInterfile
        | OutOfMemoryDuringInterfile ) ) ->
        spf "when running %s on %s" id path
    | Some id, IncompatibleRule _ -> id
    | Some id, MissingPlugin -> spf "for rule %s" id
    | _ -> spf "at line %s:%d" path location.start.line
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
  | SpecifiedParseError
  | AstBuilderError
  | RuleParseError
  | PatternParseError _
  | InvalidYaml
  | MatchingError
  | SemgrepMatchFound
  | TooManyMatches
  | FatalError
  | Timeout
  | OutOfMemory
  | TimeoutDuringInterfile
  | OutOfMemoryDuringInterfile ->
      Exit_code.fatal
  | IncompatibleRule _
  | MissingPlugin ->
      Exit_code.ok

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
        | PartialParsing _ ->
            None
        | SpecifiedParseError
        | AstBuilderError
        | RuleParseError
        | PatternParseError _
        | InvalidYaml
        | MatchingError
        | SemgrepMatchFound
        | TooManyMatches
        | FatalError
        | Timeout
        | OutOfMemory
        | TimeoutDuringInterfile
        | OutOfMemoryDuringInterfile
        | IncompatibleRule _
        | MissingPlugin ->
            rule_id
      in
      let path =
        (* # For rule errors path is a temp file so will just be confusing *)
        match error_type with
        | RuleParseError
        | PatternParseError _ ->
            None
        | ParseError
        | LexicalError
        | PartialParsing _
        | SpecifiedParseError
        | AstBuilderError
        | InvalidYaml
        | MatchingError
        | SemgrepMatchFound
        | TooManyMatches
        | FatalError
        | Timeout
        | OutOfMemory
        | TimeoutDuringInterfile
        | OutOfMemoryDuringInterfile
        | IncompatibleRule _
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

(* This is a cursed function that calculates everything but the index part
 * of the match_based_id. It's cursed because we need hashes to be exactly
 * the same, but the algorithm used on the python side to generate
 * the final string thats hashed has some python specific quirks.
 *
 * The way match based ID is calculated on the python side
 * is as follows:
 * (see https://github.com/returntocorp/semgrep/blob/2d34ce584d16c4e954349690a5f12fae877a94d6/cli/src/semgrep/rule.py#L289-L334)
 * 1. Sort all top level keys (i.e pattern, patterns etc.) alphabetically
 * 2. For each key: DFS the tree and find all pattern values (i.e. the rhs of pattern: <THING>)
 * 3. Sort all pattern values alphabetically and concatenate them with a space
 * 4. Concatenate all the sorted pattern values with a space
 * 5. Hash the tuple `(sorted_pattern_values, path, rule_id)` w/ blake2b
 * 6. Append the index of the match in the list of matches for the rule (see [index_match_based_ids])
 *
 * Austin: I wrote the initial match based ID, and this one below is a port of it.
 * Looking back it seems like I ended up writing a roundabout version of the below algorithm
 * which is how this function works
 * 1. Same as before
 * 2. for each rule: get all xpatterns, then sort them, and concatenate them with a space
 * 3. Sort all of step 2 results alphabetically and concatenate them with a space
 * 4. Same as step 5 and 6
 *
 * Assumptions:
 * Somewhere it seems that all keys are already sorted alphabetically when the rule is parsed.
 * I have not seen this code. I simply have faith that it is true and this will not change.
 *
 * I'm also hoping that [interpolate_metavariables] works similar to what we do on the python side
 *
 * I have not tested this code beyond checking a bunch of examples manually.
 *
 * There's some weird thing we do w/ join mode. I am hoping that this doesn't matter irl
 *)
let match_based_id_partial rule rule_id metavars path =
  let xpats = Rule.xpatterns_of_rule rule in
  let xpat_strs =
    xpats |> Common.map (fun (xpat : Xpattern.t) -> fst xpat.pstr)
  in
  let sorted_xpat_strs = List.sort String.compare xpat_strs in
  let xpat_str = String.concat " " sorted_xpat_strs in
  let metavars = Option.value ~default:[] metavars in
  let xpat_str_interp =
    interpolate_metavars xpat_str metavars path |> String.escaped
  in
  (* Python doesn't escape the double quote character, but ocaml does :/ so we need this monstrosity *)
  let py_esc_reg = Str.regexp "\\\\\\\"" in
  let xpat_str_interp = Str.global_replace py_esc_reg "\"" xpat_str_interp in
  (* We have been hashing w/ this PosixPath thing in python so we must recreate it here  *)
  (* We also have been hashing a tuple formatted as below *)
  let string =
    spf "('%s', PosixPath('%s'), '%s')" xpat_str_interp path rule_id
  in
  let hash = Digestif.BLAKE2B.digest_string string |> Digestif.BLAKE2B.to_hex in
  hash

let cli_match_of_core_match (hrules : Rule.hrules) (m : Out.core_match) :
    Out.cli_match =
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
       (* used now in render_fix instead *)
       rendered_fix = _;
       (* LATER *)
       dataflow_trace = _;
     };
  } ->
      let rule =
        try Hashtbl.find hrules (Rule_ID.of_string rule_id) with
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
      (* message where the metavars have been interpolated *)
      let message = interpolate_metavars message metavars path in
      let fix = render_fix hrules m in
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
        Semgrep_output_utils.lines_of_file_at_range (start, end_) (Fpath.v path)
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
            (* fields derived from the rule (and the match) *)
            message;
            severity;
            metadata;
            fix;
            (* TODO: other fields derived from the rule *)
            fix_regex = None;
            (* TODO: extra fields *)
            is_ignored = Some false;
            fingerprint = match_based_id_partial rule rule_id metavars path;
            sca_info = None;
            fixed_lines = None;
            dataflow_trace = None;
            (* It's optional in the CLI output, but not in the core match results!
             *)
            engine_kind = Some engine_kind;
            validation_state;
            extra_extra;
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
let index_match_based_ids (matches : Out.cli_match list) : Out.cli_match list =
  matches
  (* preserve order *)
  |> Common.mapi (fun i x -> (i, x))
  (* Group by rule and path *)
  |> Common.group_by (fun (_, (x : Out.cli_match)) -> (x.path, x.check_id))
  (* Sort by start line *)
  |> Common.map (fun (path_and_rule_id, matches) ->
         ( path_and_rule_id,
           List.sort
             (fun (_, (a : Out.cli_match)) (_, (b : Out.cli_match)) ->
               compare a.start.offset b.start.offset)
             matches ))
  (* Index per file *)
  |> Common.map (fun (path_and_rule_id, matches) ->
         let matches =
           Common.mapi
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
  |> Common.map snd

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* The 3 parameters are mostly Core_runner.result but we don't want
 * to depend on cli_scan/ from reporting/ here, hence the duplication.
 * alt: we could move Core_runner.result type in core/
 *)
let cli_output_of_core_results ~logging_level (core : Out.core_output)
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
   (* LATER *)
   time = _;
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
      let scanned = scanned |> Set_.elements |> File.Path.to_strings in
      let (paths : Out.scanned_and_skipped) =
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
      {
        version;
        (* Skipping the python intermediate RuleMatchMap for now.
         * TODO: handle the rule_match.cli_unique_key to dedup matches
         *)
        results =
          matches
          |> Common.map (cli_match_of_core_match hrules)
          |> dedup_and_sort;
        errors = errors |> Common.map cli_error_of_core_error;
        paths;
        skipped_rules;
        explanations;
        (* LATER *)
        time = None;
        rules_by_engine = None;
        engine_requested = None;
      }
