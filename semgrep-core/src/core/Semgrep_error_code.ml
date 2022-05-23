(*
 * Copyright (C) 2021 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
module PI = Parse_info
module Out = Output_from_core_j

let logger = Logging.get_logger [ __MODULE__ ]

(****************************************************************************)
(* Prelude *)
(****************************************************************************)
(* Error management for semgrep-core.
 *)

(****************************************************************************)
(* Types and globals *)
(****************************************************************************)

(* See also try_with_exn_to_errors(), try_with_error_loc_and_reraise(), and
 * filter_maybe_parse_and_fatal_errors.
 * less: we should define everything in Output_from_core.atd, not just typ:
 *)
type error = {
  rule_id : Rule.rule_id option;
  typ : Out.core_error_kind;
  loc : Parse_info.token_location;
  msg : string;
  details : string option;
  yaml_path : string list option;
}

(* TODO: define also in Output_from_core.atd *)
type severity = Error | Warning

let g_errors = ref []

(****************************************************************************)
(* Convertor functions *)
(****************************************************************************)

let please_file_issue_text =
  "An error occurred while invoking the Semgrep engine. Please help us fix \
   this by creating an issue at https://github.com/returntocorp/semgrep"

let mk_error ?(rule_id = None) loc msg err =
  let msg =
    match err with
    | Out.MatchingError
    | Out.AstBuilderError
    | Out.FatalError
    | Out.TooManyMatches ->
        Printf.sprintf "%s\n\n%s" please_file_issue_text msg
    | _ -> msg
  in
  { rule_id; loc; typ = err; msg; details = None; yaml_path = None }

let mk_error_tok ?(rule_id = None) tok msg err =
  let loc = PI.unsafe_token_location_of_info tok in
  mk_error ~rule_id loc msg err

let error rule_id loc msg err =
  Common.push (mk_error ~rule_id:(Some rule_id) loc msg err) g_errors

(* TODO: why not capture AST_generic.error here? So we could get rid
 * of Run_semgrep.exn_to_error wrapper.
 *)
let exn_to_error ?(rule_id = None) file exn =
  match exn with
  | Parse_info.Lexical_error (s, tok) ->
      mk_error_tok ~rule_id tok s Out.LexicalError
  | Parse_info.Parsing_error tok ->
      let msg =
        match tok with
        | { token = PI.OriginTok { str; _ }; _ } ->
            spf "`%s` was unexpected" str
        | _ -> "unknown reason"
      in
      mk_error_tok tok msg Out.ParseError
  | Parse_info.Other_error (s, tok) ->
      mk_error_tok ~rule_id tok s Out.SpecifiedParseError
  | Rule.InvalidRule
      (Rule.InvalidPattern (pattern, xlang, message, yaml_path), rule_id, pos)
    ->
      {
        rule_id = Some rule_id;
        typ = Out.PatternParseError;
        loc = PI.unsafe_token_location_of_info pos;
        msg =
          (* TODO: make message helpful *)
          spf
            "Invalid pattern for %s:\n\
             --- pattern ---\n\
             %s\n\
             --- end pattern ---\n\
             Pattern error: %s\n"
            (Xlang.to_string xlang) pattern message;
        details = None;
        yaml_path = Some yaml_path;
      }
  | Rule.InvalidRule (kind, rule_id, pos) ->
      let str = Rule.string_of_invalid_rule_error_kind kind in
      mk_error_tok ~rule_id:(Some rule_id) pos str Out.RuleParseError
  | Rule.InvalidYaml (msg, pos) -> mk_error_tok ~rule_id pos msg Out.InvalidYaml
  | Rule.DuplicateYamlKey (s, pos) ->
      mk_error_tok ~rule_id pos s Out.InvalidYaml
  | Common.Timeout timeout_info ->
      let s = Printexc.get_backtrace () in
      logger#error "WEIRD Timeout converted to exn, backtrace = %s" s;
      (* This exception should always be reraised. *)
      let loc = Parse_info.first_loc_of_file file in
      let msg = Common.string_of_timeout_info timeout_info in
      mk_error ~rule_id loc msg Out.Timeout
  | Out_of_memory ->
      let loc = Parse_info.first_loc_of_file file in
      mk_error ~rule_id loc "Heap space exceeded" Out.OutOfMemory
  | ExceededMemoryLimit msg ->
      let loc = Parse_info.first_loc_of_file file in
      mk_error ~rule_id loc msg Out.OutOfMemory
  | UnixExit _ as exn -> raise exn
  (* general case, can't extract line information from it, default to line 1 *)
  | exn ->
      let trace = Printexc.get_backtrace () in
      let loc = Parse_info.first_loc_of_file file in
      {
        rule_id;
        typ = Out.FatalError;
        loc;
        msg = Common.exn_to_s exn;
        details = Some trace;
        yaml_path = None;
      }

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

let source_of_string = function
  | "" -> "<input>"
  | path -> path

let string_of_error err =
  let pos = err.loc in
  let details =
    match err.details with
    | None -> ""
    | Some s -> spf "\n%s" s
  in
  spf "%s:%d:%d: %s: %s%s"
    (source_of_string pos.PI.file)
    pos.PI.line pos.PI.column
    (Out.string_of_core_error_kind err.typ)
    err.msg details

let severity_of_error typ =
  match typ with
  | Out.SemgrepMatchFound -> Error
  | Out.MatchingError -> Warning
  | Out.TooManyMatches -> Warning
  | Out.LexicalError -> Warning
  | Out.ParseError -> Warning
  | Out.SpecifiedParseError -> Warning
  | Out.AstBuilderError -> Error
  | Out.RuleParseError -> Error
  | Out.PatternParseError -> Error
  | Out.InvalidYaml -> Warning
  | Out.FatalError -> Error
  | Out.Timeout -> Warning
  | Out.OutOfMemory -> Warning

(*****************************************************************************)
(* Try with error *)
(*****************************************************************************)

let try_with_exn_to_error file f =
  try f () with
  | Timeout _ as exn -> raise exn
  | exn -> Common.push (exn_to_error file exn) g_errors

let try_with_print_exn_and_reraise file f =
  try f () with
  | Timeout _ as exn -> raise exn
  | exn ->
      let bt = Printexc.get_backtrace () in
      let err = exn_to_error file exn in
      pr2 (string_of_error err);
      pr2 bt;
      (* does not really re-raise :( lose some backtrace *)
      raise exn

(* fast = no stack trace *)
let try_with_print_exn_and_exit_fast file f =
  try f () with
  | Timeout _ as exn -> raise exn
  | exn ->
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
  |> Common.map (fun file ->
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

(* A copy-paste of Error_code.compare_actual_to_expected but
 * with Semgrep_error_code.error instead of Error_code.t for the error type.
 *)
let compare_actual_to_expected actual_errors expected_error_lines =
  let actual_error_lines =
    actual_errors
    |> Common.map (fun err ->
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

let compare_actual_to_expected_for_alcotest actual expected =
  match compare_actual_to_expected actual expected with
  | Ok () -> ()
  | Error (_num_errors, msg) -> Alcotest.fail msg
