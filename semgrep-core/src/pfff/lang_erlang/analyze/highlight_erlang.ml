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
open Common

open Entity_code open Highlight_code
module T = Parser_erlang
module PI = Parse_info

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
    (_program, toks)
  =
  let already_tagged = Hashtbl.create 101 in
  let atom_already_tagged = Hashtbl.create 101 in

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
         | _ when s =~ ".*=========" && s5 =~ ".*========" ->
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
    | T.TIdent (s, ii1)::xs when PI.col_of_info ii1 = 0 ->
        if not (Hashtbl.mem atom_already_tagged s) then begin
          Hashtbl.add atom_already_tagged s true;
          tag ii1 (Entity (Function, (Def2 fake_no_def2)));
        end;
        aux_toks xs

    (* uses *)
    | T.TIdent(_s, ii1)::T.TColon _::xs ->
        tag ii1 (Entity (Module, (Use2 fake_no_use2)));
        aux_toks xs

    | T.TIdent(_s, ii1)::T.TOParen _::xs ->
        tag ii1 (Entity (Function, (Use2 fake_no_use2)));
        aux_toks xs

(*
    |   T.TIdent (s1, ii1)::T.TDot ii2
      ::T.TIdent (s3, ii3)::T.TOParen(ii4)::xs ->
        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then begin
          tag ii3 (Method (Use2 fake_no_use2));
          (*
          if not (Hashtbl.mem already_tagged ii1)
          then tag ii1 (Local Use);
          *)
          if is_module_name s1 then tag ii1 (Module (Use))
        end;
        aux_toks xs

    |   T.TIdent (s1, ii1)::T.TDot ii2
      ::T.TIdent (s3, ii3)::T.TEq ii4::xs ->
        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then begin
          tag ii3 (Field (Use2 fake_no_use2));
          if is_module_name s1 then tag ii1 (Module (Use))
        end;
        aux_toks xs


    |  T.TIdent (s1, ii1)::T.TDot ii2
     ::T.TIdent (s3, ii3)::T.TDot ii4::xs ->
        if not (Hashtbl.mem already_tagged ii1) && lexer_based_tagger
        then begin
          if is_module_name s1 then tag ii1 (Module Use)
        end;
        aux_toks (T.TIdent (s3, ii3)::T.TDot ii4::xs)
*)

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
    | T.TCommentSpace ii ->
        if not (Hashtbl.mem already_tagged ii)
        then ()
        else ()
    | T.TCommentNewline _ii | T.TCommentMisc _ii -> ()
    | T.TUnknown ii -> tag ii Error
    | T.EOF _ii -> ()

    (* values  *)
    | T.TString (_s,ii) ->
        tag ii String
    | T.TChar (_s, ii) ->
        tag ii String
    | T.TFloat (_s,ii) | T.TInt (_s,ii) ->
        tag ii Number

    (* keywords  *)
    | T.Tif ii
    | T.Tcond ii
    | T.Tcase ii
    | T.Twhen ii
      -> tag ii KeywordConditional

    | T.Treceive ii
    | T.Tquery ii
    | T.Tafter ii
      -> tag ii Keyword

    | T.Tfun ii
    | T.Tlet ii
    | T.Tof ii
      -> tag ii Keyword

    | T.Tend ii |T.Tbegin ii  ->
        tag ii Keyword

    | T.Tcatch ii
      -> tag ii KeywordExn

    | T.TIdent (("module" | "include" | "export"), ii) ->
        tag ii KeywordModule

    | T.TIdent ("record", ii) ->
        tag ii Keyword
    | T.TIdent (("true" | "false"), ii) ->
        tag ii Boolean

    (* symbols *)
    | T.TEq ii ->
        if not (Hashtbl.mem already_tagged ii)
        then
          tag ii Punctuation

    | T.TOBracket ii | T.TCBracket ii
    | T.TOBrace ii | T.TCBrace ii
    | T.TOParen ii | T.TCParen ii
      -> tag ii Punctuation

    | T.TPlus ii | T.TMinus ii
    | T.TStar ii | T.TDiv ii

    | T.TLess ii | T.TMore ii
    | T.TLessEq ii | T.TMoreEq ii
    | T.TEqEq ii | T.TEqSlashEq ii| T.TEqColonEq ii | T.TSlashEq ii
      -> tag ii Operator

    | T.TDot ii
    | T.TColon ii
      ->
        tag ii Punctuation

    | T.TComma ii
      -> tag ii Punctuation

    | T.TDec ii | T.TInc ii
    | T.TBang ii
      -> tag ii Operator

    (* todo? put in pink/bad-smell ? *)
    | T.TAssign ii ->
        tag ii Operator

    | T.TArrow ii
    | T.TQuestion ii
    | T.TSemiColon ii
      -> tag ii Punctuation

    | T.Tbnot ii | T.Tnot ii
    | T.Tband ii  | T.Tand ii
    | T.Tbsr ii  | T.Tbsl ii | T.Tbxor ii | T.Tbor ii
    | T.Txor ii | T.Tor ii
    | T.Trem ii | T.Tdiv ii
      -> tag ii Operator

    | T.TUnderscore ii
    | T.TSharp ii
    | T.TPipePipe ii
    | T.TPipe ii
      -> tag ii Punctuation


    | T.TIdent (_s, _ii) ->
        ()
    | T.TVariable (_s, ii) ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii (Local Use)


  );
  (* -------------------------------------------------------------------- *)
  (* ast phase 2 *)

  ()
