(* Yoann Padioleau
 *
 * Copyright (C) 2019 Yoann Padioleau
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

open Parser_skip
module PI = Parse_info

(*****************************************************************************)
(* Token Helpers *)
(*****************************************************************************)

let is_eof = function
  | EOF _ -> true
  | _ -> false

let is_comment = function
  | TComment _ | TCommentSpace _ | TCommentNewline _ -> true
  | _ -> false

let token_kind_of_tok t =
  match t with
  | TOBrace _ -> PI.LBrace
  | TCBrace _ -> PI.RBrace
  | TOParen _ -> PI.LPar
  | TCParen _ -> PI.RPar
  (* less: also TOBracket? (and TOBracketAt...) *)

  | TComment _ -> PI.Esthet PI.Comment
  | TCommentSpace _ -> PI.Esthet PI.Space
  | TCommentNewline _ -> PI.Esthet PI.Newline

  | _ -> PI.Other

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)

let visitor_info_of_tok f = function
  | TCommentSpace ii -> TCommentSpace (f ii)
  | TCommentNewline ii -> TCommentNewline (f ii)
  | TComment ii -> TComment (f ii)
  | TUnknown ii -> TUnknown (f ii)
  | EOF ii -> EOF (f ii)

  | TQuestion ii -> TQuestion (f ii)
  | TAt ii -> TAt (f ii)
  | TSharp ii -> TSharp (f ii)
  | TDollar ii -> TDollar (f ii)
  | TBackquote ii -> TBackquote (f ii)
  | Tfor ii -> Tfor (f ii)
  | Tin ii -> Tin (f ii)
  | Textension ii -> Textension (f ii)
  | Tyield ii -> Tyield (f ii)
  | Tbreak ii -> Tbreak (f ii)
  | Tcontinue ii -> Tcontinue (f ii)
  | Tmemoized ii -> Tmemoized (f ii)
  | Tfrozen ii -> Tfrozen (f ii)
  | Tdo ii -> Tdo (f ii)
  | Twhile ii -> Twhile (f ii)
  | Tloop ii -> Tloop (f ii)

  | TEqDot ii -> TEqDot (f ii)
  | TTildeArrow ii -> TTildeArrow (f ii)
  | TEqualArrow ii -> TEqualArrow (f ii)
  | TDiv ii -> TDiv (f ii)
  | TMod ii -> TMod (f ii)
  | TEqEq ii -> TEqEq (f ii)
  | TLessEq ii -> TLessEq (f ii)
  | TGreaterEq ii -> TGreaterEq (f ii)
  | THat ii -> THat (f ii)
  | TPipePipe ii -> TPipePipe (f ii)


  | TInt (s, ii) -> TInt (s, f ii)
  | TFloat (s, ii) -> TFloat (s, f ii)
  | TChar (s, ii) -> TChar (s, f ii)
  | TString (s, ii) -> TString (s, f ii)
  | TLowerIdent (s, ii) -> TLowerIdent (s, f ii)
  | TUpperIdent (s, ii) -> TUpperIdent (s, f ii)

  | Toverridable ii -> Toverridable (f ii)
  | Treadonly ii -> Treadonly (f ii)
  | Tmacro ii -> Tmacro (f ii)

  | Talias ii -> Talias (f ii)
  | Tas ii -> Tas (f ii)
  | Tasync ii -> Tasync (f ii)
  | Tawait ii -> Tawait (f ii)
  | Tcatch ii -> Tcatch (f ii)
  | Tchildren ii -> Tchildren (f ii)
  | Tclass ii -> Tclass (f ii)
  | Tconst ii -> Tconst (f ii)
  | Telse ii -> Telse (f ii)
  | Textends ii -> Textends (f ii)
  | Tfinal ii -> Tfinal (f ii)
  | Tfrom ii -> Tfrom (f ii)
  | Tfun ii -> Tfun (f ii)
  | Tif ii -> Tif (f ii)
  | Tmatch ii -> Tmatch (f ii)
  | Tmodule ii -> Tmodule (f ii)
  | Tmutable ii -> Tmutable (f ii)
  | Tnative ii -> Tnative (f ii)
  | Tprivate ii -> Tprivate (f ii)
  | Tprotected ii -> Tprotected (f ii)
  | Tuses ii -> Tuses (f ii)
  | Tstatic ii -> Tstatic (f ii)
  | Tthis ii -> Tthis (f ii)
  | Tthrow ii -> Tthrow (f ii)
  | Ttrait ii -> Ttrait (f ii)
  | Ttry ii -> Ttry (f ii)
  | Ttype ii -> Ttype (f ii)
  | Tvoid ii -> Tvoid (f ii)
  | Twatch ii -> Twatch (f ii)
  | Twhen ii -> Twhen (f ii)
  | Twith ii -> Twith (f ii)
  | Tbase ii -> Tbase (f ii)
  | Tcapture ii -> Tcapture (f ii)
  | Tdefault ii -> Tdefault (f ii)
  | Tdeferred ii -> Tdeferred (f ii)
  | Tinst ii -> Tinst (f ii)
  | TnonNullable ii -> TnonNullable (f ii)
  | Tuntracked ii -> Tuntracked (f ii)
  | Tvalue ii -> Tvalue (f ii)
  | Ttrue ii -> Ttrue (f ii)
  | Tfalse ii -> Tfalse (f ii)


  | TOParen ii -> TOParen (f ii)
  | TCParen ii -> TCParen (f ii)
  | TOBrace ii -> TOBrace (f ii)
  | TCBrace ii -> TCBrace (f ii)
  | TOBracket ii -> TOBracket (f ii)
  | TCBracket ii -> TCBracket (f ii)

  | TLess ii -> TLess (f ii)
  | TGreater ii -> TGreater (f ii)
  | TDot ii -> TDot (f ii)

  | TComma ii -> TComma (f ii)
  | TEq ii -> TEq (f ii)

  | TColon ii -> TColon (f ii)
  | TColonColon ii -> TColonColon (f ii)
  | TBang ii -> TBang (f ii)
  | TBangEq ii -> TBangEq (f ii)

  | TPipe ii -> TPipe (f ii)
  | TSemiColon ii -> TSemiColon (f ii)
  | TSemiColonSemiColon ii -> TSemiColonSemiColon (f ii)

  | TStar ii -> TStar (f ii)
  | TArrow ii -> TArrow (f ii)

  | TAnd ii -> TAnd (f ii)
  | TAndAnd ii -> TAndAnd (f ii)

  | TPlus ii -> TPlus (f ii)
  | TMinus ii -> TMinus (f ii)

let info_of_tok tok =
  let res = ref None in
  visitor_info_of_tok (fun ii -> res := Some ii; ii) tok |> ignore;
  Common2.some !res
