open Common
module PI = Parse_info

type error = {
  typ : error_kind;
  loc : Parse_info.token_location;
  sev : severity;
}

and severity = Error | Warning | Info

and error_kind =
  (* File parsing related errors.
   * See also try_with_exn_to_errors(), try_with_error_loc_and_reraise(), and
   * filter_maybe_parse_and_fatal_errors
   *)
  | LexicalError of string
  | ParseError (* aka SyntaxError *)
  | SpecifiedParseError of string (* aka SyntaxError *)
  | AstBuilderError of string
  (* pattern parsing related errors *)
  | RuleParseError of string
  | PatternParseError of string
  | InvalidYaml of string
  (* matching (semgrep) related *)
  | MatchingError of string (* internal error, e.g., NoTokenLocation *)
  | SemgrepMatchFound of (string (* check_id *) * string) (* msg *)
  | TooManyMatches of string (* can contain offending pattern *)
  (* other *)
  | FatalError of string (* missing file, OCaml errors, etc. *)
  | Timeout of string option
  | OutOfMemory of string option

let g_errors = ref []

let options () = []

(*****************************************************************************)
(* Convertor functions *)
(****************************************************************************)

let mk_error tok err =
  let loc = PI.token_location_of_info tok in
  { loc; typ = err; sev = Error }

let mk_error_loc loc err = { loc; typ = err; sev = Error }

let error tok err = Common.push (mk_error tok err) g_errors

let error_loc loc err = Common.push (mk_error_loc loc err) g_errors

let todo =
  {
    Parse_info.token =
      Parse_info.OriginTok
        {
          charpos = -1;
          str = "";
          line = -1;
          column = -1;
          file = "FAKE TOKEN LOCATION";
        };
    transfo = NoTransfo;
  }

let exn_to_error file exn =
  match exn with
  | Parse_info.Lexical_error (s, tok) -> mk_error tok (LexicalError s)
  | Parse_info.Parsing_error tok -> mk_error tok ParseError
  | Parse_info.Other_error (s, tok) -> mk_error tok (SpecifiedParseError s)
  | Rule.InvalidRule (_rule_id, s, _posTODO) -> mk_error todo (RuleParseError s)
  | Rule.InvalidLanguage (_rule_id, language, _posTODO) ->
      mk_error todo (RuleParseError language)
  | Rule.InvalidRegexp (_rule_id, message, _posTODO) ->
      mk_error todo (RuleParseError message)
  | Rule.InvalidPattern (_rule_id, _pattern, _xlang, message, pos, _path) ->
      mk_error pos (PatternParseError message)
  | Rule.InvalidYaml (msg, _posTODO) -> mk_error todo (InvalidYaml msg)
  | Rule.DuplicateYamlKey (s, pos) -> mk_error pos (InvalidYaml s)
  | Rule.UnparsableYamlException msg -> mk_error todo (InvalidYaml msg)
  | Common.Timeout timeout_info ->
      (* This exception should always be reraised. *)
      let loc = Parse_info.first_loc_of_file file in
      let msg = Common.string_of_timeout_info timeout_info in
      mk_error_loc loc (Timeout (Some msg))
  | Out_of_memory ->
      let loc = Parse_info.first_loc_of_file file in
      mk_error_loc loc (OutOfMemory None)
  | UnixExit _ as exn -> raise exn
  (* general case, can't extract line information from it, default to line 1 *)
  | exn ->
      let loc = Parse_info.first_loc_of_file file in
      let msg =
        Common.spf "%s\n%s" (Common.exn_to_s exn) (Printexc.get_backtrace ())
      in
      mk_error_loc loc (FatalError msg)

(* TODO remove this *)
let check_id_of_error_kind = function
  | LexicalError _ -> "LexicalError"
  | ParseError -> "ParseError"
  | SpecifiedParseError _ -> "SpecifiedParseError"
  | AstBuilderError _ -> "AstBuilderError"
  (* pattern parsing related errors *)
  | RuleParseError _ -> "RuleParseError"
  | PatternParseError _ -> "PatternParseError"
  | InvalidYaml _ -> "InvalidYaml"
  (* semgrep *)
  | SemgrepMatchFound (check_id, _) -> spf "sgrep-lint-<%s>" check_id
  | MatchingError _ -> "MatchingError"
  | TooManyMatches _ -> "TooManyMatches"
  (* other *)
  | FatalError _ -> "FatalError"
  | Timeout _ -> "Timeout"
  | OutOfMemory _ -> "OutOfMemory"

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

let string_of_error_kind error_kind =
  match error_kind with
  | SemgrepMatchFound (_title, message) -> message
  | MatchingError s -> spf "matching internal error: %s" s
  | TooManyMatches s -> spf "too many matches: %s" s
  | LexicalError s -> spf "Lexical error: %s" s
  | ParseError -> "Syntax error"
  | SpecifiedParseError s -> spf "Other syntax error: %s" s
  | AstBuilderError s -> spf "AST builder error: %s" s
  | RuleParseError s -> spf "Rule parse error: %s" s
  | PatternParseError s -> spf "Pattern parse error: %s" s
  | InvalidYaml s -> spf "Invalid YAML: %s" s
  | FatalError s -> spf "Fatal Error: %s" s
  | Timeout None -> "Timeout"
  | Timeout (Some s) -> "Timeout:" ^ s
  | OutOfMemory None -> "Out of memory"
  | OutOfMemory (Some s) -> "Out of memory:" ^ s

let string_of_error err =
  let pos = err.loc in
  assert (pos.PI.file <> "");
  spf "%s:%d:%d: %s" pos.PI.file pos.PI.line pos.PI.column
    (string_of_error_kind err.typ)

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
