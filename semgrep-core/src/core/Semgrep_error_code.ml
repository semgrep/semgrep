open Common
module PI = Parse_info

type error = {
  rule_id : Rule.rule_id option;
  typ : error_kind;
  loc : Parse_info.token_location;
  msg : string;
  sev : severity;
  details : string option;
  path : string list option;
}

and severity = Error | Warning | Info

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

let g_errors = ref []

let options () = []

(****************************************************************************)
(* Helpers *)
(****************************************************************************)

let build_message typ msg =
  match typ with
  | SemgrepMatchFound _title -> msg
  | MatchingError -> spf "Internal matching error: %s" msg
  | TooManyMatches -> spf "Too many matches: %s" msg
  | LexicalError -> spf "Lexical error: %s" msg
  | ParseError -> "Syntax error"
  | SpecifiedParseError -> spf "Other syntax error: %s" msg
  | AstBuilderError -> spf "AST builder error: %s" msg
  | RuleParseError -> spf "Rule parse error: %s" msg
  | PatternParseError -> spf "Pattern parse error: %s" msg
  | InvalidYaml -> spf "Invalid YAML: %s" msg
  | FatalError -> spf "Fatal Error: %s" msg
  | Timeout -> "Timeout:" ^ msg
  | OutOfMemory -> "Out of memory:" ^ msg

(****************************************************************************)
(* Convertor functions *)
(****************************************************************************)

let mk_error rule_id loc msg err =
  let msg = build_message err msg in
  {
    rule_id = Some rule_id;
    loc;
    typ = err;
    msg;
    sev = Error;
    details = None;
    path = None;
  }

let mk_error_tok rule_id tok msg err =
  let loc = PI.token_location_of_info tok in
  mk_error rule_id loc msg err

let mk_error_no_rule loc msg err =
  let msg = build_message err msg in
  {
    rule_id = None;
    loc;
    typ = err;
    msg;
    sev = Error;
    details = None;
    path = None;
  }

let mk_error_tok_no_rule tok msg err =
  let loc = PI.token_location_of_info tok in
  mk_error_no_rule loc msg err

let error rule_id loc msg err =
  Common.push (mk_error rule_id loc msg err) g_errors

let error_tok rule_id tok msg err =
  Common.push (mk_error_tok rule_id tok msg err) g_errors

let exn_to_error file exn =
  match exn with
  | Parse_info.Lexical_error (s, tok) -> mk_error_tok_no_rule tok s LexicalError
  | Parse_info.Parsing_error tok -> mk_error_tok_no_rule tok "" ParseError
  | Parse_info.Other_error (s, tok) ->
      mk_error_tok_no_rule tok s SpecifiedParseError
  | Rule.InvalidRule (rule_id, s, pos) ->
      mk_error_tok rule_id pos s RuleParseError
  | Rule.InvalidLanguage (rule_id, language, pos) ->
      mk_error_tok rule_id pos
        (spf "invalid language %s" language)
        RuleParseError
  | Rule.InvalidRegexp (rule_id, message, pos) ->
      mk_error_tok rule_id pos (spf "invalid regex %s" message) RuleParseError
  | Rule.InvalidPattern (rule_id, _pattern, xlang, message, pos, path) ->
      {
        rule_id = Some rule_id;
        typ = PatternParseError;
        loc = PI.token_location_of_info pos;
        msg =
          spf "Invalid pattern for %s: %s" (Rule.string_of_xlang xlang) message;
        sev = Error;
        details = None;
        path = Some path;
      }
  | Rule.InvalidYaml (msg, pos) -> mk_error_tok_no_rule pos msg InvalidYaml
  | Rule.DuplicateYamlKey (s, pos) -> mk_error_tok_no_rule pos s InvalidYaml
  | Common.Timeout timeout_info ->
      (* This exception should always be reraised. *)
      let loc = Parse_info.first_loc_of_file file in
      let msg = Common.string_of_timeout_info timeout_info in
      mk_error_no_rule loc msg Timeout
  | Out_of_memory ->
      let loc = Parse_info.first_loc_of_file file in
      mk_error_no_rule loc "Out of memory" OutOfMemory
  | UnixExit _ as exn -> raise exn
  (* general case, can't extract line information from it, default to line 1 *)
  | exn ->
      let loc = Parse_info.first_loc_of_file file in
      {
        rule_id = None;
        typ = FatalError;
        loc;
        msg = Common.exn_to_s exn;
        sev = Error;
        details = Some (Printexc.get_backtrace ());
        path = None;
      }

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

let string_of_error err =
  let pos = err.loc in
  assert (pos.PI.file <> "");
  spf "%s:%d:%d: %s" pos.PI.file pos.PI.line pos.PI.column err.msg

let string_of_error_kind = function
  | LexicalError -> "LexicalError"
  | ParseError -> "ParseError"
  | SpecifiedParseError -> "SpecifiedParseError"
  | AstBuilderError -> "AstBuilderError"
  (* pattern parsing related errors *)
  | RuleParseError -> "RuleParseError"
  | PatternParseError -> "PatternParseError"
  | InvalidYaml -> "InvalidYaml"
  (* semgrep *)
  | SemgrepMatchFound check_id -> spf "sgrep-lint-<%s>" check_id
  | MatchingError -> "MatchingError"
  | TooManyMatches -> "TooManyMatches"
  (* other *)
  | FatalError -> "FatalError"
  | Timeout -> "Timeout"
  | OutOfMemory -> "OutOfMemory"

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
  match num_errors with 0 -> Stdlib.Ok () | n -> Error (n, msg)
