(* Yoann Padioleau
 *
 * Copyright (C) 2021 Semgrep Inc.
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
module Out = Semgrep_output_v1_j
module R = Rule

let logger = Logging.get_logger [ __MODULE__ ]

(****************************************************************************)
(* Prelude *)
(****************************************************************************)
(* Error management for semgrep-core.
 *
 * Note that the "core" errors are translated at some point in
 * Semgrep_output_v1.core_error, then processed in pysemgrep (or osemgrep)
 * and translated again in Semgrep_output_v1.error.
 * There's also Error.ml in osemgrep.
 * LATER: it would be good to remove some intermediate types.
 *)

(****************************************************************************)
(* Types and globals *)
(****************************************************************************)

(* See also try_with_exn_to_errors(), try_with_error_loc_and_reraise(), and
 * filter_maybe_parse_and_fatal_errors.
 * less: we should define everything in Output_from_core.atd, not just typ:
 *)
type t = {
  rule_id : Rule_ID.t option;
  typ : Out.error_type;
  loc : Tok.location;
  msg : string;
  details : string option;
}
[@@deriving show]

let g_errors = ref []

(* ugly alias because 'type t = t' is not allowed *)
type core_error = t

(* TODO: use Set_.t instead *)
module ErrorSet = Set.Make (struct
  type t = core_error

  let compare = compare
end)

(****************************************************************************)
(* Convertor functions *)
(****************************************************************************)

let please_file_issue_text =
  "An error occurred while invoking the Semgrep engine. Please help us fix \
   this by creating an issue at https://github.com/returntocorp/semgrep"

let mk_error opt_rule_id loc msg err =
  let msg =
    match (err : Out.error_type) with
    | MatchingError
    | AstBuilderError
    | FatalError
    | TooManyMatches ->
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
    | PartialParsing _
    | IncompatibleRule _
    | SemgrepError
    | InvalidRuleSchemaError
    | UnknownLanguageError
    | MissingPlugin ->
        msg
  in
  { rule_id = opt_rule_id; loc; typ = err; msg; details = None }

let mk_error_tok opt_rule_id tok msg err =
  let loc = Tok.unsafe_loc_of_tok tok in
  mk_error opt_rule_id loc msg err

let error rule_id loc msg err =
  Common.push (mk_error (Some rule_id) loc msg err) g_errors

let error_of_invalid_rule_error ((kind, rule_id, pos) : R.invalid_rule_error) :
    t =
  let msg = Rule.string_of_invalid_rule_error_kind kind in
  let err =
    match kind with
    | IncompatibleRule (this_version, (min_version, max_version)) ->
        Out.IncompatibleRule
          {
            rule_id = Rule_ID.to_string rule_id;
            this_version = Version_info.to_string this_version;
            min_version = Option.map Version_info.to_string min_version;
            max_version = Option.map Version_info.to_string max_version;
          }
    | MissingPlugin _msg -> Out.MissingPlugin
    | _ -> Out.RuleParseError
  in
  mk_error_tok (Some rule_id) pos msg err

let opt_error_of_rule_error (err : Rule.error) : t option =
  let rule_id = err.rule_id in
  match err.kind with
  | InvalidRule
      (InvalidPattern (pattern, xlang, message, yaml_path), rule_id, pos) ->
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
  | InvalidRule err -> Some (error_of_invalid_rule_error err)
  | InvalidYaml (msg, pos) ->
      Some (mk_error_tok rule_id pos msg Out.InvalidYaml)
  | DuplicateYamlKey (s, pos) ->
      Some (mk_error_tok rule_id pos s Out.InvalidYaml)
  (* TODO?? *)
  | UnparsableYamlException _ -> None

(*
   This function converts known exceptions to Semgrep errors.
   We also use it to register global exception printers for
   'Printexc.to_string' to show useful messages.

   TODO: why not capture AST_generic.error here? So we could get rid
   of Run_semgrep.exn_to_error wrapper.
*)
let known_exn_to_error rule_id file (e : Exception.t) : t option =
  match Exception.get_exn e with
  (* TODO: Move the cases handling Parsing_error.XXX to the Parsing_error
     module so that we can use it for the exception printers that are
     registered there. *)
  | Parsing_error.Lexical_error (s, tok) ->
      Some (mk_error_tok rule_id tok s Out.LexicalError)
  | Parsing_error.Syntax_error tok ->
      let msg =
        match tok with
        | Tok.OriginTok { str = ""; _ } ->
            (* TODO: at least in some cases, this comes from a MISSING node
               inserted by tree-sitter. These are reported as errors
               with a good error message that was lost.
               We should preserve the original error message. *)
            "missing element"
        | Tok.OriginTok { str; _ } -> spf "`%s` was unexpected" str
        | __else__ -> "unknown reason"
      in
      Some (mk_error_tok rule_id tok msg Out.ParseError)
  | Parsing_error.Other_error (s, tok) ->
      Some (mk_error_tok rule_id tok s Out.SpecifiedParseError)
  | Rule.Error err -> opt_error_of_rule_error err
  | Time_limit.Timeout timeout_info ->
      let s = Printexc.get_backtrace () in
      logger#error "WEIRD Timeout converted to exn, backtrace = %s" s;
      (* This exception should always be reraised. *)
      let loc = Tok.first_loc_of_file file in
      let msg = Time_limit.string_of_timeout_info timeout_info in
      Some (mk_error rule_id loc msg Out.Timeout)
  | Memory_limit.ExceededMemoryLimit msg ->
      let loc = Tok.first_loc_of_file file in
      Some (mk_error rule_id loc msg Out.OutOfMemory)
  | Out_of_memory ->
      let loc = Tok.first_loc_of_file file in
      Some (mk_error rule_id loc "Heap space exceeded" Out.OutOfMemory)
  (* general case, can't extract line information from it, default to line 1 *)
  | _exn -> None

let exn_to_error rule_id file (e : Exception.t) : t =
  match known_exn_to_error rule_id file e with
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
    (Out.string_of_error_type err.typ)
    err.msg details

let severity_of_error (typ : Out.error_type) : Out.error_severity =
  match typ with
  | SemgrepMatchFound -> `Error
  | MatchingError -> `Warning
  | TooManyMatches -> `Warning
  | LexicalError -> `Warning
  | ParseError -> `Warning
  | PartialParsing _ -> `Warning
  | SpecifiedParseError -> `Warning
  | AstBuilderError -> `Error
  | RuleParseError -> `Error
  | PatternParseError _ -> `Error
  | InvalidYaml -> `Warning
  | FatalError -> `Error
  | Timeout -> `Warning
  | OutOfMemory -> `Warning
  | TimeoutDuringInterfile -> `Error
  | OutOfMemoryDuringInterfile -> `Error
  | SemgrepError -> `Error
  | InvalidRuleSchemaError -> `Error
  | UnknownLanguageError -> `Error
  | IncompatibleRule _
  | MissingPlugin ->
      (* Running into an incompatible rule may be normal, with nothing to fix *)
      `Info

(*****************************************************************************)
(* Try with error *)
(*****************************************************************************)

let try_with_exn_to_error file f =
  try f () with
  | Time_limit.Timeout _ as exn -> Exception.catch_and_reraise exn
  | exn ->
      let e = Exception.catch exn in
      Common.push (exn_to_error None file e) g_errors

let try_with_print_exn_and_reraise file f =
  try f () with
  | Time_limit.Timeout _ as exn -> Exception.catch_and_reraise exn
  | exn ->
      let e = Exception.catch exn in
      let err = exn_to_error None file e in
      pr2 (string_of_error err);
      Exception.reraise e

(*****************************************************************************)
(* Helper functions to use in testing code *)
(*****************************************************************************)

let default_error_regexp = ".*\\(ERROR\\|MATCH\\):"

let (expected_error_lines_of_files :
      ?regexp:string ->
      ?ok_regexp:string option ->
      Common.filename list ->
      (Common.filename * int) (* line *) list) =
 fun ?(regexp = default_error_regexp) ?(ok_regexp = None) test_files ->
  test_files
  |> List.concat_map (fun file ->
         Common.cat file |> Common.index_list_1
         |> Common.map_filter (fun (s, idx) ->
                (* Right now we don't care about the actual error messages. We
                 * don't check if they match. We are just happy to check for
                 * correct lines error reporting.
                 *)
                if
                  s =~ regexp (* + 1 because the comment is one line before *)
                  (* This is so that we can mark a line differently for OSS and Pro,
                     e.g. `ruleid: deepok: example_rule_id` *)
                  && Option.fold ~none:true
                       ~some:(fun ok_regexp -> not (s =~ ok_regexp))
                       ok_regexp
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
