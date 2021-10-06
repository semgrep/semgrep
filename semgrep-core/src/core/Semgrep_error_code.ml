open Common
module PI = Parse_info

type error = {
  rule_id : Rule.rule_id option;
  typ : error_kind;
  loc : Parse_info.token_location;
  msg : string;
  details : string option;
  yaml_path : string list option;
}

and error_kind =
  (* File parsing related errors.
   * See also try_with_exn_to_errors(), try_with_error_loc_and_reraise(), and
   * filter_maybe_parse_and_fatal_errors
   *)
  | LexicalError
  | ParseError (* aka SyntaxError *)
  | SpecifiedParseError
  | AstBuilderError
  (* pattern parsing related errors *)
  | RuleParseError
  | PatternParseError
  | InvalidYaml
  (* matching (semgrep) related *)
  | MatchingError (* internal error, e.g., NoTokenLocation *)
  | SemgrepMatchFound of string (* check_id *)
  | TooManyMatches
  (* other *)
  | FatalError (* missing file, OCaml errors, etc. *)
  | Timeout
  | OutOfMemory

type severity = Error | Warning

let g_errors = ref []

let options () = []

(****************************************************************************)
(* Convertor functions *)
(****************************************************************************)

let mk_error ?(rule_id = None) loc msg err =
  { rule_id; loc; typ = err; msg; details = None; yaml_path = None }

let mk_error_tok ?(rule_id = None) tok msg err =
  let loc = PI.unsafe_token_location_of_info tok in
  mk_error ~rule_id loc msg err

let error rule_id loc msg err =
  Common.push (mk_error ~rule_id:(Some rule_id) loc msg err) g_errors

let error_tok rule_id tok msg err =
  Common.push (mk_error_tok ~rule_id:(Some rule_id) tok msg err) g_errors

let exn_to_error ?(rule_id = None) file exn =
  match exn with
  | Parse_info.Lexical_error (s, tok) ->
      mk_error_tok ~rule_id tok s LexicalError
  | Parse_info.Parsing_error tok ->
      let msg =
        match tok with
        | { token = PI.OriginTok { str; _ }; _ } ->
            spf "`%s` was unexpected" str
        | _ -> "unknown reason"
      in
      mk_error_tok tok msg ParseError
  | Parse_info.Other_error (s, tok) ->
      mk_error_tok ~rule_id tok s SpecifiedParseError
  | Rule.InvalidRule (rule_id, s, pos) ->
      mk_error_tok ~rule_id:(Some rule_id) pos s RuleParseError
  | Rule.InvalidLanguage (rule_id, language, pos) ->
      mk_error_tok ~rule_id:(Some rule_id) pos
        (spf "invalid language %s" language)
        RuleParseError
  | Rule.InvalidRegexp (rule_id, message, pos) ->
      mk_error_tok ~rule_id:(Some rule_id) pos
        (spf "invalid regex %s" message)
        RuleParseError
  | Rule.InvalidPattern (rule_id, _pattern, xlang, _message, pos, yaml_path) ->
      {
        rule_id = Some rule_id;
        typ = PatternParseError;
        loc = PI.unsafe_token_location_of_info pos;
        msg =
          (* TODO: make message helpful *)
          spf "Invalid pattern for %s" (Rule.string_of_xlang xlang);
        details = None;
        yaml_path = Some yaml_path;
      }
  | Rule.InvalidYaml (msg, pos) -> mk_error_tok ~rule_id pos msg InvalidYaml
  | Rule.DuplicateYamlKey (s, pos) -> mk_error_tok ~rule_id pos s InvalidYaml
  | Common.Timeout timeout_info ->
      (* This exception should always be reraised. *)
      let loc = Parse_info.first_loc_of_file file in
      let msg = Common.string_of_timeout_info timeout_info in
      mk_error ~rule_id loc msg Timeout
  | Out_of_memory ->
      let loc = Parse_info.first_loc_of_file file in
      mk_error ~rule_id loc "Heap space exceeded" OutOfMemory
  | ExceededMemoryLimit msg ->
      let loc = Parse_info.first_loc_of_file file in
      mk_error ~rule_id loc msg OutOfMemory
  | UnixExit _ as exn -> raise exn
  (* general case, can't extract line information from it, default to line 1 *)
  | exn ->
      let loc = Parse_info.first_loc_of_file file in
      {
        rule_id;
        typ = FatalError;
        loc;
        msg = Common.exn_to_s exn;
        details = Some (Printexc.get_backtrace ());
        yaml_path = None;
      }

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

let string_of_error_kind = function
  | LexicalError -> "Lexical error"
  | ParseError -> "Syntax error"
  | SpecifiedParseError -> "Other syntax error"
  | AstBuilderError -> "AST builder error"
  (* pattern parsing related errors *)
  | RuleParseError -> "Rule parse error"
  | PatternParseError -> "Pattern parse error"
  | InvalidYaml -> "Invalid YAML"
  (* semgrep *)
  | SemgrepMatchFound check_id ->
      (* TODO: please make the error message obvious to the user *)
      spf "Semgrep match found '%s'" check_id
  | MatchingError -> "Internal matching error"
  | TooManyMatches -> "Too many matches"
  (* other *)
  | FatalError -> "Fatal error"
  | Timeout -> "Timeout"
  | OutOfMemory -> "Out of memory"

let string_of_error err =
  let pos = err.loc in
  assert (pos.PI.file <> "");
  let details =
    match err.details with
    | None -> ""
    | Some s -> spf "\n%s" s
  in
  spf "%s:%d:%d: %s: %s%s" pos.PI.file pos.PI.line pos.PI.column
    (string_of_error_kind err.typ)
    err.msg details

let severity_of_error typ =
  match typ with
  | SemgrepMatchFound _title -> Warning
  | MatchingError -> Warning
  | TooManyMatches -> Warning
  | LexicalError -> Warning
  | ParseError -> Warning
  | SpecifiedParseError -> Warning
  | AstBuilderError -> Error
  | RuleParseError -> Error
  | PatternParseError -> Error
  | InvalidYaml -> Warning
  | FatalError -> Error
  | Timeout -> Warning
  | OutOfMemory -> Warning

(*****************************************************************************)
(* Try with error *)
(*****************************************************************************)

let try_with_exn_to_error file f =
  try f () with exn -> Common.push (exn_to_error file exn) g_errors

let try_with_print_exn_and_reraise file f =
  try f ()
  with exn ->
    let bt = Printexc.get_backtrace () in
    let err = exn_to_error file exn in
    pr2 (string_of_error err);
    pr2 bt;
    (* does not really re-raise :( lose some backtrace *)
    raise exn

(* fast = no stack trace *)
let try_with_print_exn_and_exit_fast file f =
  try f ()
  with exn ->
    let err = exn_to_error file exn in
    pr2 (string_of_error err);
    exit 2

(*****************************************************************************)
(* Helper functions to use in testing code *)
(*****************************************************************************)

let default_error_regexp = ".*\\(ERROR\\|MATCH\\):"

let (expected_error_lines_of_files :
      ?regexp:string ->
      Common.filename list ->
      (Common.filename * int) (* line *) list) =
 fun ?(regexp = default_error_regexp) test_files ->
  test_files
  |> List.map (fun file ->
         Common.cat file |> Common.index_list_1
         |> Common.map_filter (fun (s, idx) ->
                (* Right now we don't care about the actual error messages. We
                 * don't check if they match. We are just happy to check for
                 * correct lines error reporting.
                 *)
                if s =~ regexp (* + 1 because the comment is one line before *)
                then Some (file, idx + 1)
                else None))
  |> List.flatten

let compare_actual_to_expected actual_errors expected_error_lines =
  let actual_error_lines =
    actual_errors
    |> List.map (fun err ->
           let loc = err.loc in
           (loc.PI.file, loc.PI.line))
  in
  (* diff report *)
  let _common, only_in_expected, only_in_actual =
    Common2.diff_set_eff expected_error_lines actual_error_lines
  in

  only_in_expected
  |> List.iter (fun (src, l) ->
         pr2 (spf "this one error is missing: %s:%d" src l));
  only_in_actual
  |> List.iter (fun (src, l) ->
         pr2
           (spf "this one error was not expected: %s:%d (%s)" src l
              (actual_errors
              |> List.find (fun err ->
                     let loc = err.loc in
                     src =$= loc.PI.file && l =|= loc.PI.line)
              |> string_of_error)));
  let num_errors = List.length only_in_actual + List.length only_in_expected in
  let msg =
    spf "it should find all reported errors and no more (%d errors)" num_errors
  in
  match num_errors with
  | 0 -> Stdlib.Ok ()
  | n -> Error (n, msg)
