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

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type token =
  | TComment of Tok.t
  | TCommentSpace of Tok.t
  | TCommentNewline of Tok.t
  | TNumber of (string * Tok.t)
  | TIdent of (string * Tok.t)
  | TString of (string * Tok.t)
  | TOParen of Tok.t
  | TCParen of Tok.t
  | TOBracket of Tok.t
  | TCBracket of Tok.t
  | TQuote of Tok.t
  (* anti-quote expressions tokens, as in `(foo ,v ,@xs) *)
  | TBackQuote of Tok.t
  | TComma of Tok.t
  | TAt of Tok.t
  | TUnknown of Tok.t
  | EOF of Tok.t

(*****************************************************************************)
(* Token Helpers *)
(*****************************************************************************)

let is_eof = function
  | EOF _ -> true
  | _ -> false

let is_comment = function
  | TComment _
  | TCommentSpace _
  | TCommentNewline _ ->
      true
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
  visitor_info_of_tok
    (fun ii ->
      res := Some ii;
      ii)
    tok
  |> ignore;
  Common2.some !res

let str_of_tok x = Tok.content_of_tok (info_of_tok x)
