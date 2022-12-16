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

open Parser_csharp

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

  | Tbool ii -> Tbool (f ii)
  | Tbyte ii -> Tbyte (f ii)
  | Tchar ii -> Tchar (f ii)
  | Tvoid ii -> Tvoid (f ii)
  | Tdouble ii -> Tdouble (f ii)
  | Tfloat ii -> Tfloat (f ii)
  | Tshort ii -> Tshort (f ii)
  | Tint ii -> Tint (f ii)
  | Tlong ii -> Tlong (f ii)
  | Tstring ii -> Tstring (f ii)
  | Tsbyte ii -> Tsbyte (f ii)
  | Tushort ii -> Tushort (f ii)
  | Tuint ii -> Tuint (f ii)
  | Tulong ii -> Tulong (f ii)
  | Tclass ii -> Tclass (f ii)
  | Tabstract ii -> Tabstract (f ii)
  | Tvirtual ii -> Tvirtual (f ii)
  | Tdelegate ii -> Tdelegate (f ii)
  | Tthis ii -> Tthis (f ii)
  | Tinterface ii -> Tinterface (f ii)
  | Tnew ii -> Tnew (f ii)
  | Tobject ii -> Tobject (f ii)
  | Tprivate ii -> Tprivate (f ii)
  | Tprotected ii -> Tprotected (f ii)
  | Tpublic ii -> Tpublic (f ii)
  | Treturn ii -> Treturn (f ii)
  | Tbreak ii -> Tbreak (f ii)
  | Tcontinue ii -> Tcontinue (f ii)
  | Tswitch ii -> Tswitch (f ii)
  | Tcase ii -> Tcase (f ii)
  | Tdefault ii -> Tdefault (f ii)
  | Tenum ii -> Tenum (f ii)
  | Tstruct ii -> Tstruct (f ii)
  | Tconst ii -> Tconst (f ii)
  | Tunsafe ii -> Tunsafe (f ii)
  | Tnamespace ii -> Tnamespace (f ii)
  | Tusing ii -> Tusing (f ii)
  | Tstatic ii -> Tstatic (f ii)
  | Tvolatile ii -> Tvolatile (f ii)
  | Textern ii -> Textern (f ii)
  | Tif ii -> Tif (f ii)
  | Telse ii -> Telse (f ii)
  | Tdo ii -> Tdo (f ii)
  | Twhile ii -> Twhile (f ii)
  | Tfor ii -> Tfor (f ii)
  | Tforeach ii -> Tforeach (f ii)
  | Tgoto ii -> Tgoto (f ii)
  | Tthrow ii -> Tthrow (f ii)
  | Ttry ii -> Ttry (f ii)
  | Tcatch ii -> Tcatch (f ii)
  | Tfinally ii -> Tfinally (f ii)
  | Tchecked ii -> Tchecked (f ii)
  | Tunchecked ii -> Tunchecked (f ii)
  | Tnull ii -> Tnull (f ii)
  | Ttrue ii -> Ttrue (f ii)
  | Tfalse ii -> Tfalse (f ii)
  | Tref ii -> Tref (f ii)
  | Tout ii -> Tout (f ii)
  | Tas ii -> Tas (f ii)
  | Tbase ii -> Tbase (f ii)
  | Tdecimal ii -> Tdecimal (f ii)
  | Tevent ii -> Tevent (f ii)
  | Texplicit ii -> Texplicit (f ii)
  | Tfixed ii -> Tfixed (f ii)
  | Timplicit ii -> Timplicit (f ii)
  | Tin ii -> Tin (f ii)
  | Tinternal ii -> Tinternal (f ii)
  | Tis ii -> Tis (f ii)
  | Tlock ii -> Tlock (f ii)
  | Toperator ii -> Toperator (f ii)
  | Toverride ii -> Toverride (f ii)
  | Tparams ii -> Tparams (f ii)
  | Treadonly ii -> Treadonly (f ii)
  | Tsealed ii -> Tsealed (f ii)
  | Tsizeof ii -> Tsizeof (f ii)
  | Tstackalloc ii -> Tstackalloc (f ii)
  | Ttypeof ii -> Ttypeof (f ii)
  | TCppLine ii -> TCppLine (f ii)
  | TCppError ii -> TCppError (f ii)
  | TCppWarning ii -> TCppWarning (f ii)
  | TCppRegion ii -> TCppRegion (f ii)
  | TCppEndRegion ii -> TCppEndRegion (f ii)
  | TDefine ii -> TDefine (f ii)
  | TUndef ii -> TUndef (f ii)
  | TIfdefIf ii -> TIfdefIf (f ii)
  | TIfdefElif ii -> TIfdefElif (f ii)
  | TIfdefElse ii -> TIfdefElse (f ii)
  | TIfdefEndif ii -> TIfdefEndif (f ii)
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
  | TDot ii -> TDot (f ii)
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
  | TQuestion ii -> TQuestion (f ii)
  | TInc ii -> TInc (f ii)
  | TDec ii -> TDec (f ii)
  | TBang ii -> TBang (f ii)
  | TAndAnd ii -> TAndAnd (f ii)
  | TOrOr ii -> TOrOr (f ii)
  | TArrow ii -> TArrow (f ii)

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
