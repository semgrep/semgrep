(*
 * Copyright (C) 2021 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
module Out = Output_from_core_j
module R = Rule

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
  loc : Tok.location;
  msg : string;
  details : string option;
}
[@@deriving show]

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
    | LexicalError
    | ParseError
    | SpecifiedParseError
    | RuleParseError
    | InvalidYaml
    | SemgrepMatchFound
    | Timeout
    | OutOfMemory
    | TimeoutDuringInterfile
    | OutOfMemoryDuringInterfile
    | PatternParseError _
    | PartialParsing _ ->
        msg
  in
  { rule_id; loc; typ = err; msg; details = None }

let mk_error_tok ?(rule_id = None) tok msg err =
  let loc = Tok.unsafe_loc_of_tok tok in
  mk_error ~rule_id loc msg err

let error rule_id loc msg err =
  Common.push (mk_error ~rule_id:(Some rule_id) loc msg err) g_errors

(*
   This function converts known exceptions to Semgrep errors.
   We also use it to register global exception printers for
   'Printexc.to_string' to show useful messages.

   TODO: why not capture AST_generic.error here? So we could get rid
   of Run_semgrep.exn_to_error wrapper.

   TODO: register exception printers in their modules of origin
   (using Printexc.register_printer).
 *)
let known_exn_to_error ?(rule_id = None) file (e : Exception.t) : error option =
  match Exception.get_exn e with
  | Parsing_error.Lexical_error (s, tok) ->
      Some (mk_error_tok ~rule_id tok s Out.LexicalError)
  | Parsing_error.Syntax_error tok ->
      let msg =
        match tok with
        | Tok.OriginTok { str; _ } -> spf "`%s` was unexpected" str
        | __else__ -> "unknown reason"
      in
      Some (mk_error_tok tok msg Out.ParseError)
  | Parsing_error.Other_error (s, tok) ->
      Some (mk_error_tok ~rule_id tok s Out.SpecifiedParseError)
  | R.Err err -> (
      match err with
      | R.InvalidRule
          (R.InvalidPattern (pattern, xlang, message, yaml_path), rule_id, pos)
        ->
          Some
            {
              rule_id = Some rule_id;
              typ = Out.PatternParseError yaml_path;
              loc = Tok.unsafe_loc_of_tok pos;
              msg =
                spf
                  "Invalid pattern for %s:\n\
                   --- pattern ---\n\
                   %s\n\
                   --- end pattern ---\n\
                   Pattern error: %s\n"
                  (Xlang.to_string xlang) pattern message;
              details = None;
            }
      | R.InvalidRule (kind, rule_id, pos) ->
          let str = Rule.string_of_invalid_rule_error_kind kind in
          Some (mk_error_tok ~rule_id:(Some rule_id) pos str Out.RuleParseError)
      | R.InvalidYaml (msg, pos) ->
          Some (mk_error_tok ~rule_id pos msg Out.InvalidYaml)
      | R.DuplicateYamlKey (s, pos) ->
          Some (mk_error_tok ~rule_id pos s Out.InvalidYaml)
      (* TODO?? *)
      | R.UnparsableYamlException _ -> None)
  | Time_limit.Timeout timeout_info ->
      let s = Printexc.get_backtrace () in
      logger#error "WEIRD Timeout converted to exn, backtrace = %s" s;
      (* This exception should always be reraised. *)
      let loc = Tok.first_loc_of_file file in
      let msg = Time_limit.string_of_timeout_info timeout_info in
      Some (mk_error ~rule_id loc msg Out.Timeout)
  | Memory_limit.ExceededMemoryLimit msg ->
      let loc = Tok.first_loc_of_file file in
      Some (mk_error ~rule_id loc msg Out.OutOfMemory)
  | Out_of_memory ->
      let loc = Tok.first_loc_of_file file in
      Some (mk_error ~rule_id loc "Heap space exceeded" Out.OutOfMemory)
  (* general case, can't extract line information from it, default to line 1 *)
  | _exn -> None

let exn_to_error ?(rule_id = None) file (e : Exception.t) : error =
  match known_exn_to_error ~rule_id file e with
  | Some err -> err
  | None -> (
      match Exception.get_exn e with
      | UnixExit _ ->
          (* TODO: remove this.
             This exception shouldn't be passed to this function
             in the first place. *)
          Exception.reraise e
      | exn ->
          let trace = Exception.to_string e in
          let loc = Tok.first_loc_of_file file in
          {
            rule_id;
            typ = Out.FatalError;
            loc;
            msg = Printexc.to_string exn;
            details = Some trace;
          })

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
    (source_of_string pos.Tok.pos.file)
    pos.Tok.pos.line pos.Tok.pos.column
    (Out.string_of_core_error_kind err.typ)
    err.msg details

let severity_of_error typ =
  match typ with
  | Out.SemgrepMatchFound -> Out.Error
  | Out.MatchingError -> Warning
  | Out.TooManyMatches -> Warning
  | Out.LexicalError -> Warning
  | Out.ParseError -> Warning
  | Out.PartialParsing _ -> Warning
  | Out.SpecifiedParseError -> Warning
  | Out.AstBuilderError -> Error
  | Out.RuleParseError -> Error
  | Out.PatternParseError _ -> Error
  | Out.InvalidYaml -> Warning
  | Out.FatalError -> Error
  | Out.Timeout -> Warning
  | Out.OutOfMemory -> Warning
  | Out.TimeoutDuringInterfile -> Error
  | Out.OutOfMemoryDuringInterfile -> Error

(*****************************************************************************)
(* Try with error *)
(*****************************************************************************)

let try_with_exn_to_error file f =
  try f () with
  | Time_limit.Timeout _ as exn -> Exception.catch_and_reraise exn
  | exn ->
      let e = Exception.catch exn in
      Common.push (exn_to_error file e) g_errors

let try_with_print_exn_and_reraise file f =
  try f () with
  | Time_limit.Timeout _ as exn -> Exception.catch_and_reraise exn
  | exn ->
      let e = Exception.catch exn in
      let err = exn_to_error file e in
      pr2 (string_of_error err);
      Exception.reraise e

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
  |> List.concat_map (fun file ->
         Common.cat file |> Common.index_list_1
         |> Common.map_filter (fun (s, idx) ->
                (* Right now we don't care about the actual error messages. We
                 * don't check if they match. We are just happy to check for
                 * correct lines error reporting.
                 *)
                if s =~ regexp (* + 1 because the comment is one line before *)
                then Some (file, idx + 1)
                else None))

(* A copy-paste of Error_code.compare_actual_to_expected but
 * with Semgrep_error_code.error instead of Error_code.t for the error type.
 *)
let compare_actual_to_expected actual_findings expected_findings_lines =
  let actual_findings_lines =
    actual_findings
    |> Common.map (fun err ->
           let loc = err.loc in
           (loc.Tok.pos.file, loc.Tok.pos.line))
  in
  (* diff report *)
  let _common, only_in_expected, only_in_actual =
    Common2.diff_set_eff expected_findings_lines actual_findings_lines
  in

  only_in_expected
  |> List.iter (fun (src, l) ->
         pr2 (spf "this one finding is missing: %s:%d" src l));
  only_in_actual
  |> List.iter (fun (src, l) ->
         pr2
           (spf "this one finding was not expected: %s:%d (%s)" src l
              (actual_findings
              (* nosemgrep: ocaml.lang.best-practice.list.list-find-outside-try *)
              |> List.find (fun err ->
                     let loc = err.loc in
                     src = loc.Tok.pos.file && l =|= loc.Tok.pos.line)
              |> string_of_error)));
  let num_errors = List.length only_in_actual + List.length only_in_expected in
  let msg =
    spf "it should find all reported findings and no more (%d errors)"
      num_errors
  in
  match num_errors with
  | 0 -> Stdlib.Ok ()
  | n -> Error (n, msg)

let compare_actual_to_expected_for_alcotest actual expected =
  match compare_actual_to_expected actual expected with
  | Ok () -> ()
  | Error (_num_errors, msg) -> Alcotest.fail msg
