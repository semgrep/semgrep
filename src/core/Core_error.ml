(* Yoann Padioleau
 *
 * Copyright (C) 2021-2023 Semgrep Inc.
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
open Fpath_.Operators
module Out = Semgrep_output_v1_j
module R = Rule
module Log = Log_semgrep.Log

(****************************************************************************)
(* Prelude *)
(****************************************************************************)
(* Error management for semgrep-core.
 *
 * Note that the "core" errors are translated at some point in
 * Semgrep_output_v1.core_error, then processed in pysemgrep (or osemgrep)
 * and translated again in Semgrep_output_v1.error.
 * There's also Error.ml in osemgrep.
 *
 * LATER: it would be good to remove some intermediate types.
 *)

(****************************************************************************)
(* Types and globals *)
(****************************************************************************)

(* See also try_with_exn_to_errors(), try_with_error_loc_and_reraise(), and
 * filter_maybe_parse_and_fatal_errors.
 * less: we should define everything in semgrep_output_v1.atd, not just typ:
 *)
type t = {
  typ : Out.error_type;
  (* TODO: get rid of option.
   * Is None when we call error_of_rule_error() or exn_to_error() with an
   * Fpath_.fake_file
   * TODO: get rid of Fpath_.fake_file call sites
   *)
  loc : Tok.location option;
  msg : string;
  details : string option;
  rule_id : Rule_ID.t option;
}
[@@deriving show]

(* ugly alias because 'type t = t' is not allowed *)
type core_error = t

exception Unhandled_core_error of t

let () =
  Printexc.register_printer (function
    | Unhandled_core_error core_error ->
        Some
          (Printf.sprintf "Core_error.Unhandled_core_error(%s)"
             (show core_error))
    | _ -> None)

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

let mk_error ?rule_id ?(msg = "") (loc : Tok.location) (err : Out.error_type) :
    t =
  let msg =
    match err with
    | MatchingError
    | AstBuilderError
    | FatalError
    | TooManyMatches ->
        Printf.sprintf "%s\n\n%s" please_file_issue_text msg
    | LexicalError
    | ParseError
    | OtherParseError
    | RuleParseError
    | InvalidYaml
    | SemgrepMatchFound
    | Timeout
    | OutOfMemory
    | StackOverflow
    | TimeoutDuringInterfile
    | OutOfMemoryDuringInterfile
    | PatternParseError _
    | PatternParseError0
    | PartialParsing _
    | IncompatibleRule _
    | IncompatibleRule0
    | SemgrepWarning
    | SemgrepError
    | InvalidRuleSchemaError
    | UnknownLanguageError
    | MissingPlugin ->
        msg
  in
  { loc = Some loc; typ = err; msg; details = None; rule_id }

let mk_error_tok opt_rule_id (file : Fpath.t) (tok : Tok.t) (msg : string)
    (err : Out.error_type) : t =
  let loc =
    match Tok.loc_of_tok tok with
    | Ok loc -> loc
    | Error _ -> Tok.first_loc_of_file !!file
  in
  mk_error ?rule_id:opt_rule_id ~msg loc err

let error_of_invalid_rule ((kind, rule_id, pos) : Rule_error.invalid_rule) : t =
  let msg = Rule_error.string_of_invalid_rule_kind kind in
  let err =
    match kind with
    | IncompatibleRule (this_version, (min_version, max_version)) ->
        Out.IncompatibleRule
          {
            rule_id;
            this_version = Semver.to_string this_version;
            min_version = Option.map Semver.to_string min_version;
            max_version = Option.map Semver.to_string max_version;
          }
    | MissingPlugin _msg -> Out.MissingPlugin
    | _ -> Out.RuleParseError
  in
  (* TODO: bad use of fake_file, use pos? *)
  mk_error_tok (Some rule_id) Fpath_.fake_file pos msg err

let error_of_rule_error (file : Fpath.t) (err : Rule_error.t) : t =
  let rule_id = err.rule_id in
  match err.kind with
  | InvalidRule
      (InvalidPattern (pattern, xlang, message, yaml_path), rule_id, pos) ->
      {
        rule_id = Some rule_id;
        typ = Out.PatternParseError yaml_path;
        (* TODO: Switch to using option and report better info for figuring out why the location is missing *)
        loc = Some (Tok.unsafe_loc_of_tok pos);
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
  | InvalidRule err -> error_of_invalid_rule err
  | InvalidYaml (msg, pos) -> mk_error_tok rule_id file pos msg Out.InvalidYaml
  | DuplicateYamlKey (s, pos) -> mk_error_tok rule_id file pos s Out.InvalidYaml
  (* TODO?? *)
  | UnparsableYamlException msg ->
      (* Based on what previously happened based on exn_to_error logic before
         converting Rule parsing errors to not be exceptions. *)
      if not (Fpath_.is_fake_file file) then
        mk_error ?rule_id ~msg
          (Tok.first_loc_of_file !!file)
          Out.OtherParseError
      else
        { loc = None; typ = Out.OtherParseError; msg; details = None; rule_id }

(*
   This function converts known exceptions to Semgrep errors.
   We also use it to register global exception printers for
   'Printexc.to_string' to show useful messages.

   See also JSON_report.json_of_exn for non-target related exn handling.

   invariant: every target-related semgrep-specific exn that has a
   Parse_info.t should be captured here for precise location in error
   reporting.
   - TODO: naming exns?
*)
let known_exn_to_error (rule_id : Rule_ID.t option) (file : Fpath.t)
    (e : Exception.t) : t option =
  match Exception.get_exn e with
  (* TODO: Move the cases handling Parsing_error.XXX to the Parsing_error
     module so that we can use it for the exception printers that are
     registered there. *)
  | Parsing_error.Lexical_error (s, tok) ->
      Some (mk_error_tok rule_id file tok s Out.LexicalError)
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
      Some (mk_error_tok rule_id file tok msg Out.ParseError)
  | Parsing_error.Other_error (s, tok) ->
      Some (mk_error_tok rule_id file tok s Out.OtherParseError)
  | AST_generic.Error (s, tok) ->
      Some (mk_error_tok rule_id file tok s Out.AstBuilderError)
  | Time_limit.Timeout timeout_info ->
      let s = Printexc.get_backtrace () in
      Log.warn (fun m -> m "WEIRD Timeout converted to exn, backtrace = %s" s);
      (* This exception should always be reraised. *)
      let loc = Tok.first_loc_of_file !!file in
      let msg = Time_limit.string_of_timeout_info timeout_info in
      Some (mk_error ?rule_id ~msg loc Out.Timeout)
  | Memory_limit.ExceededMemoryLimit msg ->
      let loc = Tok.first_loc_of_file !!file in
      Some (mk_error ?rule_id ~msg loc Out.OutOfMemory)
  | Out_of_memory ->
      let loc = Tok.first_loc_of_file !!file in
      Some (mk_error ?rule_id ~msg:"Heap space exceeded" loc Out.OutOfMemory)
  (* general case, can't extract line information from it, default to line 1 *)
  | _exn -> None

let exn_to_error (rule_id : Rule_ID.t option) (file : Fpath.t) (e : Exception.t)
    : t =
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
          let loc =
            (* TODO: we shouldn't build Tok.t w/out a filename, but
               lets do it here so we don't crash until we do *)
            if not (Fpath_.is_fake_file file) then
              Some (Tok.first_loc_of_file !!file)
            else None
          in
          {
            rule_id;
            (* bugfix: we used to return [Out.FatalError] here, but pysemgrep
             * has some special handling for such error and aborts
             * aggressively the scan and display a scary stack trace.
             * We are probably here because of an unhandled exn
             * in one of the parser (e.g., Failure "not a program") but
             * we can recover from it, so let's generate a OtherParseError
             * instead.
             *)
            typ = Out.OtherParseError;
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
  let loc =
    match pos with
    | None -> "<unknown location>"
    | Some { pos = { file; line; column; _ }; _ } ->
        spf "%s:%d:%d" (source_of_string !!file) line column
  in
  spf "%s: %s: %s%s" loc (Out.string_of_error_type err.typ) err.msg details

let severity_of_error (typ : Out.error_type) : Out.error_severity =
  match typ with
  | SemgrepMatchFound -> `Error
  | MatchingError -> `Warning
  | TooManyMatches -> `Warning
  | LexicalError -> `Warning
  | ParseError -> `Warning
  | PartialParsing _ -> `Warning
  | OtherParseError -> `Warning
  | SemgrepWarning -> `Warning
  | AstBuilderError -> `Error
  | RuleParseError -> `Error
  | PatternParseError _
  | PatternParseError0 ->
      `Error
  | InvalidYaml -> `Warning
  | FatalError -> `Error
  | Timeout -> `Warning
  | OutOfMemory -> `Warning
  | StackOverflow -> `Warning
  | TimeoutDuringInterfile -> `Error
  | OutOfMemoryDuringInterfile -> `Error
  | SemgrepError -> `Error
  | InvalidRuleSchemaError -> `Error
  | UnknownLanguageError -> `Error
  (* Running into an incompatible rule may be normal, with nothing to fix *)
  | IncompatibleRule _
  | IncompatibleRule0 ->
      `Info
  | MissingPlugin -> `Info

(*****************************************************************************)
(* Try with error, mostly used in testing code *)
(*****************************************************************************)

let try_with_result_to_error (file : Fpath.t) f =
  try f () with
  | Time_limit.Timeout _ as exn -> Exception.catch_and_reraise exn
  | exn ->
      let e = Exception.catch exn in
      Error (exn_to_error None file e)

let try_with_log_exn_and_reraise (file : Fpath.t) f =
  try f () with
  | Time_limit.Timeout _ as exn -> Exception.catch_and_reraise exn
  | exn ->
      let e = Exception.catch exn in
      let err = exn_to_error None file e in
      (* nosemgrep: no-logs-in-library *)
      Logs.err (fun m -> m "%s" (string_of_error err));
      Exception.reraise e
