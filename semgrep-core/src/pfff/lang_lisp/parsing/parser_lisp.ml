(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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
module PI = Parse_info

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type token =
  | TComment of (Ast_lisp.info)
  | TCommentSpace of (Ast_lisp.info)
  | TCommentNewline of (Ast_lisp.info)

  | TNumber of (string * Ast_lisp.info)
  | TIdent of (string * Ast_lisp.info)
  | TString of (string * Ast_lisp.info)

  | TOParen of (Ast_lisp.info)
  | TCParen of (Ast_lisp.info)
  | TOBracket of (Ast_lisp.info)
  | TCBracket of (Ast_lisp.info)

  | TQuote of (Ast_lisp.info)
  (* anti-quote expressions tokens, as in `(foo ,v ,@xs) *)
  | TBackQuote of (Ast_lisp.info)
  | TComma  of (Ast_lisp.info)
  | TAt  of (Ast_lisp.info)

  | TUnknown of (Ast_lisp.info)
  | EOF of (Ast_lisp.info)

(*****************************************************************************)
(* Token Helpers *)
(*****************************************************************************)

let is_eof = function
  | EOF _ -> true
  | _ -> false

let is_comment = function
  | TComment _ | TCommentSpace _ | TCommentNewline _ -> true
  | _ -> false

let is_just_comment = function
  | TComment _ -> true
  | _ -> false

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)
let visitor_info_of_tok f = function
  | TComment ii -> TComment (f ii)
  | TCommentSpace ii -> TCommentSpace (f ii)
  | TCommentNewline ii -> TCommentNewline (f ii)

  | TNumber (s, ii) -> TNumber (s, f ii)
  | TIdent (s, ii) -> TIdent (s, f ii)
  | TString (s, ii) -> TString (s, f ii)

  | TOParen ii -> TOParen (f ii)
  | TCParen ii -> TCParen (f ii)
  | TOBracket ii -> TOBracket (f ii)
  | TCBracket ii -> TCBracket (f ii)

  | TQuote ii -> TQuote (f ii)
  | TBackQuote ii -> TBackQuote (f ii)
  | TComma ii -> TComma (f ii)
  | TAt ii -> TAt (f ii)

  | TUnknown ii -> TUnknown (f ii)
  | EOF ii -> EOF (f ii)

let info_of_tok tok =
  let res = ref None in
  visitor_info_of_tok (fun ii -> res := Some ii; ii) tok |> ignore;
  Common2.some !res


let str_of_tok  x = PI.str_of_info  (info_of_tok x)
