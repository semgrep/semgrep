(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2023 r2c
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Errors during parsing in the general sense (lexing, syntax,
 * building an AST, etc.).
 *
 * The exns in this module can be used in the different parsers in
 * Semgrep (especially in the ocamllex/menhir-based parsers).
 *
 * Note that those exns can be converted in Semgrep_error_code.error with
 * Core_error.exn_to_error()
 * coupling: Core_error.exn_to_error()
 *
 * related code:
 *  - Semgrep_output_v1.core_error_kind
 *  - Semgrep_error_code.ml
 *)

(*****************************************************************************)
(* Exns *)
(*****************************************************************************)

(* see also Parsing.Parse_error and Failure "empty token" raised by Lexing *)
exception Lexical_error of string * Tok.t

(* better than Parsing.Parse_error, which does not have location information *)
exception Syntax_error of Tok.t

(* when we convert a CST to AST *)
exception Ast_builder_error of string * Tok.t

(* other errors during parsing.
 * TODO? use Tok.location instead of Tok.t?
 *)
exception Other_error of string * Tok.t

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* val lexical_error : string -> Lexing.lexbuf -> unit *)
let lexical_error msg lexbuf =
  let info = Tok.tok_of_lexbuf lexbuf in
  if !Flag_parsing.exn_when_lexical_error then raise (Lexical_error (msg, info))
  else if !Flag_parsing.verbose_lexing then UCommon.pr2_once ("LEXER: " ^ msg)
  else ()

(****************************************************************************)
(* Exception printers for Printexc.to_string *)
(****************************************************************************)
(*
   Register printers for the exceptions defined in this module.

   This makes 'Printexc.to_string' print the exceptions in a more complete
   fashion than the default printer, which only prints ints and strings
   and doesn't descend any deeper.
*)

(* TODO: move tok Tok.ml *)
let shorten_string s =
  if String.length s > 200 then String.sub s 0 200 ^ " ... (truncated)" else s

(*
   For error messages.
   - should be useful to a human reader
   - should not raise an exception
*)
let show_token_value (x : Tok.t) : string =
  match x with
  | OriginTok loc -> spf "%S" (shorten_string loc.str)
  | FakeTok (fake, _opt_loc) -> spf "fake %S" (shorten_string fake)
  | ExpandedTok (first_loc, _) ->
      (* not sure about this *)
      spf "%S" (shorten_string first_loc.str)
  | Ab -> "abstract token"

let show_token_value_and_location (x : Tok.t) =
  let location = Tok.stringpos_of_tok x in
  let value = show_token_value x in
  spf "%s %s" location value

let string_of_exn e =
  let p = show_token_value_and_location in
  match e with
  (* TODO?  | NoTokenLocation msg -> Some (spf "Parse_info.NoTokenLocation (%s)" msg) *)
  | Lexical_error (msg, tok) ->
      Some (spf "Parsing_error.Lexical_error (%s, %s)" msg (p tok))
  | Syntax_error tok -> Some (spf "Parsing_error.Syntax_error (%s)" (p tok))
  | Ast_builder_error (msg, tok) ->
      Some (spf "Parsing_error.Ast_builder_error (%s, %s)" msg (p tok))
  | Other_error (msg, tok) ->
      Some (spf "Parsing_error.Other_error (%s, %s)" msg (p tok))
  | _ -> None

(* It's appropriate to register the exception printers here because they
   were freshly defined and nobody expects other printers to be active. *)
let () = Printexc.register_printer string_of_exn
