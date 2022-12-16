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
open Parser_erlang

(*****************************************************************************)
(* Token Helpers *)
(*****************************************************************************)

let is_eof = function
  | EOF _ -> true
  | _ -> false

let is_comment = function
  | TComment _ | TCommentSpace _ | TCommentNewline _ -> true
  | TCommentMisc _ -> true
  | _ -> false

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)

let visitor_info_of_tok f = function
  | TCommentSpace ii -> TCommentSpace (f ii)
  | TCommentNewline ii -> TCommentNewline (f ii)
  | TComment ii -> TComment (f ii)
  | TCommentMisc ii -> TCommentMisc (f ii)

  | TInt (s, ii) -> TInt (s, f ii)
  | TFloat (s, ii) -> TFloat (s, f ii)
  | TChar (s, ii) -> TChar (s, f ii)
  | TString (s, ii) -> TString (s, f ii)
  | TIdent (s, ii) -> TIdent (s, f ii)
  | TVariable (s, ii) -> TVariable (s, f ii)

  | Tif ii -> Tif (f ii)
  | Tcond ii -> Tcond (f ii)
  | Twhen ii -> Twhen (f ii)
  | Tcase ii -> Tcase (f ii)
  | Tbegin ii -> Tbegin (f ii)
  | Tend ii -> Tend (f ii)
  | Tlet ii -> Tlet (f ii)
  | Tof ii -> Tof (f ii)
  | Tfun ii -> Tfun (f ii)
  | Tafter ii -> Tafter (f ii)
  | Tquery ii -> Tquery (f ii)
  | Tcatch ii -> Tcatch (f ii)
  | Treceive ii -> Treceive (f ii)
  | TOParen ii -> TOParen (f ii)
  | TCParen ii -> TCParen (f ii)
  | TOBracket ii -> TOBracket (f ii)
  | TCBracket ii -> TCBracket (f ii)
  | TOBrace ii -> TOBrace (f ii)
  | TCBrace ii -> TCBrace (f ii)
  | TDot ii -> TDot (f ii)
  | TColon ii -> TColon (f ii)
  | TSemiColon ii -> TSemiColon (f ii)
  | TComma ii -> TComma (f ii)
  | TQuestion ii -> TQuestion (f ii)
  | TPipe ii -> TPipe (f ii)
  | TPipePipe ii -> TPipePipe (f ii)
  | TArrow ii -> TArrow (f ii)
  | TSharp ii -> TSharp (f ii)
  | TUnderscore ii -> TUnderscore (f ii)
  | TPlus ii -> TPlus (f ii)
  | TMinus ii -> TMinus (f ii)
  | TStar ii -> TStar (f ii)
  | TDiv ii -> TDiv (f ii)
  | Tdiv ii -> Tdiv (f ii)
  | Trem ii -> Trem (f ii)
  | Tor ii -> Tor (f ii)
  | Txor ii -> Txor (f ii)
  | Tbor ii -> Tbor (f ii)
  | Tbxor ii -> Tbxor (f ii)
  | Tbsl ii -> Tbsl (f ii)
  | Tbsr ii -> Tbsr (f ii)
  | Tand ii -> Tand (f ii)
  | Tband ii -> Tband (f ii)
  | Tnot ii -> Tnot (f ii)
  | Tbnot ii -> Tbnot (f ii)
  | TEqEq ii -> TEqEq (f ii)
  | TSlashEq ii -> TSlashEq (f ii)
  | TEqColonEq ii -> TEqColonEq (f ii)
  | TEqSlashEq ii -> TEqSlashEq (f ii)
  | TLess ii -> TLess (f ii)
  | TMore ii -> TMore (f ii)
  | TLessEq ii -> TLessEq (f ii)
  | TMoreEq ii -> TMoreEq (f ii)
  | TInc ii -> TInc (f ii)
  | TDec ii -> TDec (f ii)
  | TEq ii -> TEq (f ii)
  | TBang ii -> TBang (f ii)
  | TAssign ii -> TAssign (f ii)


  | TUnknown ii -> TUnknown (f ii)
  | EOF ii -> EOF (f ii)


let info_of_tok tok =
  let res = ref None in
  visitor_info_of_tok (fun ii -> res := Some ii; ii) tok |> ignore;
  Common2.some !res
