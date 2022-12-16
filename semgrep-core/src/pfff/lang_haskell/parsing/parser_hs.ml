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
  | TComment of (Parse_info.t)
  | TCommentSpace of (Parse_info.t)
  | TCommentNewline of (Parse_info.t)

  | TNumber of (string * Parse_info.t)
  | TString of (string * Parse_info.t)
  | TChar of (string * Parse_info.t)

  | TIdent of (string * Parse_info.t)
  | TUpperIdent of (string * Parse_info.t)
  | TSymbol of (string * Parse_info.t)

  | TOParen of (Parse_info.t)
  | TCParen of (Parse_info.t)
  | TOBracket of (Parse_info.t)
  | TCBracket of (Parse_info.t)
  | TOBrace of (Parse_info.t)
  | TCBrace of (Parse_info.t)

  | TComma of (Parse_info.t)
  | TSemiColon of (Parse_info.t)
  | TPipe of (Parse_info.t)

  | Tdata of (Parse_info.t)
  | Tnewtype of (Parse_info.t)
  | Ttype of (Parse_info.t)

  | Tclass of (Parse_info.t)
  | Tinstance of (Parse_info.t)
  | Tdefault of (Parse_info.t)
  | Tderiving of (Parse_info.t)
  | Tdo of (Parse_info.t)
  | Tif of (Parse_info.t)
  | Tthen of (Parse_info.t)
  | Telse of (Parse_info.t)
  | Tcase of (Parse_info.t)
  | Tof of (Parse_info.t)
  | Tmodule of (Parse_info.t)
  | Timport of (Parse_info.t)
  | Tlet of (Parse_info.t)
  | Tin of (Parse_info.t)
  | Twhere of (Parse_info.t)
  | Tinfix of (Parse_info.t)
  | Tinfixl of (Parse_info.t)
  | Tinfixr of (Parse_info.t)

  | Tqualified of (Parse_info.t)
  | Tas of (Parse_info.t)
  | Thiding of (Parse_info.t)

  | TUnknown of (Parse_info.t)
  | EOF of (Parse_info.t)

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
  | TString (s, ii) -> TString (s, f ii)
  | TChar (s, ii) -> TChar (s, f ii)

  | TIdent (s, ii) -> TIdent (s, f ii)
  | TUpperIdent (s, ii) -> TUpperIdent (s, f ii)
  | TSymbol (s, ii) -> TSymbol (s, f ii)

  | TOParen ii -> TOParen (f ii)
  | TCParen ii -> TCParen (f ii)
  | TOBracket ii -> TOBracket (f ii)
  | TCBracket ii -> TCBracket (f ii)
  | TOBrace ii -> TOBrace (f ii)
  | TCBrace ii -> TCBrace (f ii)

  | TPipe ii -> TPipe (f ii)
  | TSemiColon ii -> TSemiColon (f ii)

  | TComma ii -> TComma (f ii)

  | Tdata ii -> Tdata (f ii)
  | Tnewtype ii -> Tnewtype (f ii)
  | Ttype ii -> Ttype (f ii)
  | Tclass ii -> Tclass (f ii)
  | Tinstance ii -> Tinstance (f ii)
  | Tdefault ii -> Tdefault (f ii)
  | Tderiving ii -> Tderiving (f ii)
  | Tdo ii -> Tdo (f ii)
  | Tif ii -> Tif (f ii)
  | Tthen ii -> Tthen (f ii)
  | Telse ii -> Telse (f ii)
  | Tcase ii -> Tcase (f ii)
  | Tof ii -> Tof (f ii)
  | Tmodule ii -> Tmodule (f ii)
  | Timport ii -> Timport (f ii)
  | Tlet ii -> Tlet (f ii)
  | Tin ii -> Tin (f ii)
  | Twhere ii -> Twhere (f ii)
  | Tinfix ii -> Tinfix (f ii)
  | Tinfixl ii -> Tinfixl (f ii)
  | Tinfixr ii -> Tinfixr (f ii)

  | Tqualified ii -> Tqualified (f ii)
  | Tas ii -> Tas (f ii)
  | Thiding ii -> Thiding (f ii)


  | TUnknown ii -> TUnknown (f ii)
  | EOF ii -> EOF (f ii)

let info_of_tok tok =
  let res = ref None in
  visitor_info_of_tok (fun ii -> res := Some ii; ii) tok |> ignore;
  Common2.some !res


let str_of_tok  x = PI.str_of_info  (info_of_tok x)
