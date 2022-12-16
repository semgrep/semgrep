(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
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

open Parser_rust

module PI = Parse_info

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

  | Tas(ii) -> Tas(f ii)
  | Tbox(ii) -> Tbox(f ii)
  | Tbreak(ii) -> Tbreak(f ii)
  | Tcontinue(ii) -> Tcontinue(f ii)
  | Tcrate(ii) -> Tcrate(f ii)
  | Telse(ii) -> Telse(f ii)
  | Tenum(ii) -> Tenum(f ii)
  | Textern(ii) -> Textern(f ii)
  | Tfalse(ii) -> Tfalse(f ii)
  | Tfn(ii) -> Tfn(f ii)
  | Tfor(ii) -> Tfor(f ii)
  | Tif(ii) -> Tif(f ii)
  | Timpl(ii) -> Timpl(f ii)
  | Tin(ii) -> Tin(f ii)
  | Tlet(ii) -> Tlet(f ii)
  | Tloop(ii) -> Tloop(f ii)
  | Tmatch(ii) -> Tmatch(f ii)
  | Tmod(ii) -> Tmod(f ii)
  | Tmut(ii) -> Tmut(f ii)
  | Tpriv(ii) -> Tpriv(f ii)
  | Tproc(ii) -> Tproc(f ii)
  | Tpub(ii) -> Tpub(f ii)
  | Tref(ii) -> Tref(f ii)
  | Treturn(ii) -> Treturn(f ii)
  | Tself(ii) -> Tself(f ii)
  | Tstatic(ii) -> Tstatic(f ii)
  | Tstruct(ii) -> Tstruct(f ii)
  | Tsuper(ii) -> Tsuper(f ii)
  | Ttrue(ii) -> Ttrue(f ii)
  | Ttrait(ii) -> Ttrait(f ii)
  | Ttype(ii) -> Ttype(f ii)
  | Tunsafe(ii) -> Tunsafe(f ii)
  | Tuse(ii) -> Tuse(f ii)
  | Twhile(ii) -> Twhile(f ii)



  | TCppLine ii -> TCppLine (f ii)

  | TOParen ii -> TOParen (f ii)
  | TCParen ii -> TCParen (f ii)
  | TOBracket ii -> TOBracket (f ii)
  | TCBracket ii -> TCBracket (f ii)
  | TOBrace ii -> TOBrace (f ii)
  | TCBrace ii -> TCBrace (f ii)
  | TOAngle ii -> TOAngle (f ii)
  | TCAngle ii -> TCAngle (f ii)
  | TComma ii -> TComma (f ii)
  | TColon ii -> TColon (f ii)
  | TColonColon ii -> TColonColon (f ii)
  | TSemiColon ii -> TSemiColon (f ii)
  | TStar ii -> TStar (f ii)
  | TDiv ii -> TDiv (f ii)
  | TPercent ii -> TPercent (f ii)
  | TEq ii -> TEq (f ii)
  | TEqEq ii -> TEqEq (f ii)
  | TNotEq ii -> TNotEq (f ii)
  | TPlus ii -> TPlus (f ii)
  | TMinus ii -> TMinus (f ii)
  | TTilde ii -> TTilde (f ii)
  | TAnd ii -> TAnd (f ii)
  | TOr ii -> TOr (f ii)
  | TXor ii -> TXor (f ii)
  | TLess ii -> TLess (f ii)
  | TMore ii -> TMore (f ii)
  | TMoreEq ii -> TMoreEq (f ii)
  | TLessEq ii -> TLessEq (f ii)
  | TAndAnd ii -> TAndAnd (f ii)
  | TOrOr ii -> TOrOr (f ii)
  | TArrow ii -> TArrow (f ii)
  | TDot ii -> TDot (f ii)
  | TPound ii -> TPound (f ii)

  | TAssignOp (s, ii) -> TAssignOp (s, f ii)

  | TUnknown ii -> TUnknown (f ii)
  | EOF ii -> EOF (f ii)



let info_of_tok tok =
  let res = ref None in
  visitor_info_of_tok (fun ii -> res := Some ii; ii) tok |> ignore;
  Common2.some !res

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let linecol_of_tok tok =
  let info = info_of_tok tok in
  PI.line_of_info info, PI.col_of_info info

let line_of_tok x = fst (linecol_of_tok x)

let str_of_tok  x = PI.str_of_info  (info_of_tok x)
let file_of_tok x = PI.file_of_info (info_of_tok x)
let pos_of_tok x =  PI.pos_of_info (info_of_tok x)
