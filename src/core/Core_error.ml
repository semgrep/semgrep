(* Yoann Padioleau
 *
 * Copyright (C) 2021-2024 Semgrep Inc.
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
 * LATER: it would be good to remove some intermediate types.
 *)

(****************************************************************************)
(* Types and globals *)
(****************************************************************************)

(* less: we should define everything in semgrep_output_v1.atd, not just typ:
 * coupling: almost identical to semgrep_output_v1.core_error
 *)
type t = {
  typ : Out.error_type;
  msg : string;
  (* This is None when we captured a fatal error that can't be
   * attached to a file. See exn_to_error() for example.
   *)
  loc : Tok.location option;
  rule_id : Rule_ID.t option;
  (* TODO? diff with msg? *)
  details : string option;
}
[@@deriving show]

(* Used only in pro in Deep_scan_phases.ml
 * TODO? we should probably get rid of it
 *)
exception Unhandled_core_error of t

let () =
  Printexc.register_printer (function
    | Unhandled_core_error core_error ->
        Some
          (Printf.sprintf "Core_error.Unhandled_core_error(%s)"
             (show core_error))
    | _ -> None)

(* ugly alias because 'type t = t' is not allowed in ErrorSet below *)
type core_error = t

(* TODO: use Set_.t instead *)
module ErrorSet = Set.Make (struct
  type t = core_error

  let compare = compare
end)

(****************************************************************************)
(* Error builder *)
(****************************************************************************)

let please_file_issue_text =
  "An error occurred while invoking the Semgrep engine. Please help us fix \
   this by creating an issue at https://github.com/semgrep/semgrep"

let mk_error ?rule_id ?(msg = "") ?(loc : Tok.location option)
    (err : Out.error_type) : t =
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
    | MissingPlugin
    | DependencyResolutionError _ ->
        msg
  in
  { loc; typ = err; msg; details = None; rule_id }

(* Why a file in addition to tok? Can't we just use Tok.file_of_tok on it?
 * Because in some cases this may be a fake tok with a wrong file,
 * and in some situation the caller might know the file we are currently
 * processing hence the extra file parameter.
 * TODO: this is still complicated, we should get rid of file and
 * enforce that every tok has a correct file pos.
 *)
let mk_error_tok ?rule_id ?(file : Fpath.t option) (tok : Tok.t) (msg : string)
    (err : Out.error_type) : t =
  let loc =
    match Tok.loc_of_tok tok with
    | Ok loc -> Some loc
    | Error _ ->
        let* file = file in
        Some (Tok.first_loc_of_file file)
  in
  mk_error ?rule_id ~msg ?loc err

(****************************************************************************)
(* Error of xxx *)
(****************************************************************************)

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
  mk_error_tok ~rule_id pos msg err

let error_of_rule_error (err : Rule_error.t) : t =
  let rule_id = err.rule_id in
  let file = err.file in
  match err.kind with
  | InvalidRule
      (InvalidPattern (pattern, analyzer, message, yaml_path), rule_id, pos) ->
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
            (Analyzer.to_string analyzer)
            pattern message;
        details = None;
      }
  | InvalidRule err -> error_of_invalid_rule err
  | InvalidYaml (msg, pos) ->
      mk_error_tok ?rule_id ~file pos msg Out.InvalidYaml
  | DuplicateYamlKey (s, pos) ->
      mk_error_tok ?rule_id ~file pos s Out.InvalidYaml
  | UnparsableYamlException msg ->
      mk_error ?rule_id ~msg
        ~loc:(Tok.first_loc_of_file file)
        Out.OtherParseError

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
let known_exn_to_error ?(file : Fpath.t option) (e : Exception.t) : t option =
  match Exception.get_exn e with
  (* TODO: Move the cases handling Parsing_error.XXX to the Parsing_error
     module so that we can use it for the exception printers that are
     registered there. *)
  | Parsing_error.Lexical_error (s, tok) ->
      Some (mk_error_tok ?file tok s Out.LexicalError)
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
      Some (mk_error_tok ?file tok msg Out.ParseError)
  | Parsing_error.Other_error (s, tok) ->
      Some (mk_error_tok ?file tok s Out.OtherParseError)
  | AST_generic.Error (s, tok) ->
      Some (mk_error_tok ?file tok s Out.AstBuilderError)
  | Time_limit.Timeout timeout_info ->
      let s = Printexc.get_backtrace () in
      Log.warn (fun m -> m "WEIRD Timeout converted to exn, backtrace = %s" s);
      (* This exception should always be reraised. *)
      let loc =
        let* file = file in
        Some (Tok.first_loc_of_file file)
      in
      let msg = Time_limit.string_of_timeout_info timeout_info in
      Some (mk_error ~msg ?loc Out.Timeout)
  | Memory_limit.ExceededMemoryLimit msg ->
      let loc =
        let* file = file in
        Some (Tok.first_loc_of_file file)
      in
      Some (mk_error ~msg ?loc Out.OutOfMemory)
  | Out_of_memory ->
      let loc =
        let* file = file in
        Some (Tok.first_loc_of_file file)
      in
      Some (mk_error ~msg:"Heap space exceeded" ?loc Out.OutOfMemory)
  | Common.ErrorOnFile (s, file) ->
      let loc = Some (Tok.first_loc_of_file file) in
      (* TODO: see the comment below we want OtherErrorWithAttachedFile *)
      Some (mk_error ~msg:s ?loc Out.OtherParseError)
  (* general case, can't extract line information from it, default to line 1 *)
  | _exn -> None

(* TODO: remove the file parameter and instead rewrap exns in the caller
 * using Common.ErrorOnFile
 *)
let exn_to_error ?(file : Fpath.t option) (e : Exception.t) : t =
  match known_exn_to_error ?file e with
  | Some err -> err
  | None ->
      let exn = Exception.get_exn e in
      let trace = Exception.to_string e in
      let loc =
        let* file = file in
        Some (Tok.first_loc_of_file file)
      in
      let typ =
        match file with
        (* if an exn occurs at a place where we are able to attach a file to it
         * (e.g., in Core_scan.iter_targets_xxx), which probably means only
         * one file was concerned by the error, then we should generate
         * an error with a Warning severity (see severity_of_error() further
         * below) to avoid making the whole scan fail with exit code 2
         * (see also test_semgrep_core_error.py).
         * TODO: ideally we should introduce a new OtherErrorWithAttachedFile
         * instead of abusing OtherParseError.
         *)
        | Some _ -> Out.OtherParseError
        | None -> Out.FatalError
      in
      {
        rule_id = None;
        typ;
        loc;
        msg = Printexc.to_string exn;
        details = Some trace;
      }

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

let string_of_error (err : t) : string =
  let details =
    match err.details with
    | None -> ""
    | Some s -> spf "\n%s" s
  in
  let loc =
    match err.loc with
    | None -> "<unknown location>"
    | Some { pos = { file; line; column; _ }; _ } ->
        spf "%s:%d:%d" !!file line column
  in
  spf "%s: %s: %s%s" loc (Out.string_of_error_type err.typ) err.msg details

(****************************************************************************)
(* Misc *)
(****************************************************************************)

(* Note that the difference between Error and Warning is important because
 * pysemgrep/osemgrep will return an exit code of 2 for any semgrep-core
 * Error happening during a scan (which will make the whole scan fail
 * if used in CI). Warning will generate also an exit code of 2 but only
 * if the user ran a scan with --strict.
 *)
let severity_of_error (typ : Out.error_type) : Out.error_severity =
  match typ with
  (* Warnings *)
  | MatchingError
  | TooManyMatches
  | LexicalError
  | ParseError
  | PartialParsing _
  | OtherParseError
  | InvalidYaml
  | Timeout
  | OutOfMemory
  | StackOverflow
  | SemgrepWarning ->
      `Warning
  (* Errors *)
  | SemgrepMatchFound
  | AstBuilderError
  | RuleParseError
  | PatternParseError _
  | PatternParseError0
  | TimeoutDuringInterfile
  | OutOfMemoryDuringInterfile
  | SemgrepError
  | InvalidRuleSchemaError
  | UnknownLanguageError
  | FatalError ->
      `Error
  (* Running into an incompatible rule may be normal, with nothing to fix *)
  | IncompatibleRule _
  | IncompatibleRule0
  | MissingPlugin
  | DependencyResolutionError _ ->
      `Info
