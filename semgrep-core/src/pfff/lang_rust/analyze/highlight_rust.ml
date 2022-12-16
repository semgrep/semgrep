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
open Common

open Entity_code open Highlight_code
module T = Parser_rust

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* we generate fake value here because the real one are computed in a
 * later phase in rewrite_categ_using_entities in pfff_visual.
*)
let fake_no_def2 = NoUse
let fake_no_use2 = (NoInfoPlace, UniqueDef, MultiUse)

let lexer_based_tagger = true

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

(* The idea of the code below is to visit the program either through its
 * AST or its list of tokens. The tokens are easier for tagging keywords,
 * number and basic entities. The Ast is better for tagging idents
 * to figure out what kind of ident it is.
*)

let visit_program
    ~tag_hook
    _prefs
    (*db_opt *)
    (_ast, toks)
  =
  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.replace already_tagged ii true
  )
  in

  (* -------------------------------------------------------------------- *)
  (* ast phase 1 *)

  (* -------------------------------------------------------------------- *)
  (* toks phase 1 *)

  let rec aux_toks xs =
    match xs with
    | [] -> ()
    (* a little bit pad specific *)
    |   T.TComment(ii)
        ::T.TCommentNewline _ii2
        ::T.TComment(ii3)
        ::T.TCommentNewline _ii4
        ::T.TComment(ii5)
        ::xs ->
        let s = Parse_info.str_of_info ii in
        let s5 =  Parse_info.str_of_info ii5 in
        (match () with
         | _ when s =~ ".*\\*\\*\\*\\*" && s5 =~ ".*\\*\\*\\*\\*" ->
             tag ii CommentEstet;
             tag ii5 CommentEstet;
             tag ii3 CommentSection0
         | _ when s =~ ".*------" && s5 =~ ".*------" ->
             tag ii CommentEstet;
             tag ii5 CommentEstet;
             tag ii3 CommentSection1
         | _ when s =~ ".*####" && s5 =~ ".*####" ->
             tag ii CommentEstet;
             tag ii5 CommentEstet;
             tag ii3 CommentSection2
         | _ ->
             ()
        );
        aux_toks xs

    (* poor's man identifier tagger *)

    (* defs *)
    | (T.Tmod _ )::T.TIdent (_s, ii2)::xs ->
        if not (Hashtbl.mem already_tagged ii2) && lexer_based_tagger
        then tag ii2 (Entity (Module, (Def2 fake_no_def2)));
        aux_toks xs

    | (T.Tstruct _ | T.Ttrait _ | T.Timpl _)::T.TIdent (_s, ii2)::xs ->
        if not (Hashtbl.mem already_tagged ii2) && lexer_based_tagger
        then tag ii2 (Entity (Class, (Def2 fake_no_def2)));
        aux_toks xs

    | (T.Ttype _ | T.Tenum _)::T.TIdent (_s, ii2)::xs ->
        if not (Hashtbl.mem already_tagged ii2) && lexer_based_tagger
        then tag ii2 (Entity (Type, Def2 fake_no_def2));
        aux_toks xs

    | (T.Tfn _)::T.TIdent (_s, ii2)::xs ->
        if not (Hashtbl.mem already_tagged ii2) && lexer_based_tagger
        then tag ii2 (Entity (Function, (Def2 fake_no_def2)));
        aux_toks xs

    | (T.Tlet _)::T.TIdent (_s, ii2)::xs ->
        if not (Hashtbl.mem already_tagged ii2) && lexer_based_tagger
        then tag ii2 (Local (Def));
        aux_toks xs
    | (T.Tlet _)::T.Tmut _::T.TIdent (_s, ii2)::xs ->
        if not (Hashtbl.mem already_tagged ii2) && lexer_based_tagger
        then tag ii2 (Local (Def));
        aux_toks xs


    (* uses *)

    | T.TIdent (_, ii0)::T.TColonColon ii1::T.TIdent (s2, ii2)::xs ->
        tag ii0 (Entity (Module, (Use2 fake_no_use2)));
        aux_toks (T.TColonColon ii1::T.TIdent (s2, ii2)::xs)

    | T.TColonColon _::T.TIdent (_s3, ii3)::T.TOParen _::xs ->
        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then tag ii3 (Entity (Function, (Use2 fake_no_use2)));
        aux_toks xs

    | T.TDot _::T.TIdent (_s3, ii3)::T.TOParen _::xs ->
        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then tag ii3 (Entity (Method, (Use2 fake_no_use2)));
        aux_toks xs

    | T.TIdent (_s3, ii3)::T.TOParen _::xs ->
        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then tag ii3 (Entity (Function, (Use2 fake_no_use2)));
        aux_toks xs

    | T.TDot _::T.TIdent (_s3, ii3)::xs ->
        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then tag ii3 (Entity (Field, (Use2 fake_no_use2)));
        aux_toks xs

    | (T.TArrow _) ::T.TIdent (_s3, ii3)::xs ->
        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then tag ii3 (Entity (Type, (Use2 fake_no_use2)));
        aux_toks xs

    | _x::xs ->
        aux_toks xs
  in
  let toks' = toks |> Common.exclude (function
    | T.TCommentSpace _ -> true
    | _ -> false
  )
  in
  aux_toks toks';

  (* -------------------------------------------------------------------- *)
  (* toks phase 2 *)

  toks |> List.iter (fun tok ->
    match tok with

    (* comments *)

    | T.TComment ii ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii Comment
    | T.TCommentSpace _ii -> ()
    | T.TCommentNewline _ii | T.TCommentMisc _ii -> ()
    | T.TUnknown ii -> tag ii Error
    | T.EOF _ii -> ()

    (* values  *)

    | T.TString (_s,ii) -> tag ii String
    | T.TChar (_s, ii) -> tag ii String
    | T.TFloat (_s,ii) | T.TInt (_s,ii) -> tag ii Number

    (* keywords  *)

    | T.Tstruct ii | T.Ttrait ii | T.Timpl ii
    | T.Tself ii | T.Tsuper ii
      -> tag ii KeywordObject

    | T.Tpub ii| T.Tpriv ii -> tag ii Keyword

    | T.Treturn ii | T.Tbreak ii | T.Tcontinue ii -> tag ii Keyword

    | T.Tmatch ii
      -> tag ii KeywordConditional

    | T.Tcrate ii
    | T.Tuse ii
    | T.Tmod ii
      -> tag ii KeywordModule

    | T.Tstatic ii
    | T.Textern ii
      -> tag ii Keyword

    | T.Tif ii  | T.Telse ii -> tag ii KeywordConditional
    | T.Twhile ii | T.Tfor ii | T.Tloop ii -> tag ii KeywordLoop

    | T.Ttrue ii | T.Tfalse ii ->  tag ii Boolean

    | T.Tenum ii
    | T.Ttype ii
      -> tag ii Keyword

    | T.Tlet ii
    | T.Tfn ii
    | T.Tas ii
    | T.Tin ii
      -> tag ii Keyword

    | T.Tproc ii
      -> tag ii Keyword

    | T.Tmut ii
      -> tag ii UseOfRef

    | T.Tref ii
    | T.Tbox ii
      -> tag ii UseOfRef

    | T.Tunsafe ii
      -> tag ii BadSmell


    | T.TCppLine ii
      -> tag ii CppOther

    (* symbols *)
    | T.TEq ii ->
        if not (Hashtbl.mem already_tagged ii)
        then
          tag ii Punctuation

    | T.TOBracket ii | T.TCBracket ii
    | T.TOBrace ii | T.TCBrace ii
    | T.TOParen ii | T.TCParen ii

    | T.TOAngle ii | T.TCAngle ii

    | T.TArrow ii
    | T.TDot ii
    | T.TColonColon ii
    | T.TPound ii
    | T.TColon ii
    | T.TComma ii
    | T.TSemiColon ii

      -> tag ii Punctuation

    | T.TPlus ii | T.TMinus ii
    | T.TStar ii | T.TDiv ii
    | T.TPercent ii

    | T.TLess ii | T.TMore ii

    | T.TTilde ii

    | T.TAnd ii | T.TOr ii | T.TXor ii
    | T.TAndAnd ii | T.TOrOr ii

    | T.TAssignOp (_, ii)

    | T.TNotEq ii
    | T.TLessEq ii
    | T.TMoreEq ii
    | T.TEqEq ii
      -> tag ii Operator

    | T.TIdent (s, ii) ->
        if not (Hashtbl.mem already_tagged ii)
        then
          if s =~ "^[A-Z].*" && false (* some false positive with types *)
          then tag ii (Entity (Constructor,(Use2 fake_no_use2)))
          else ()
  );
  (* -------------------------------------------------------------------- *)
  (* ast phase 2 *)

  ()
