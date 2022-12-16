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

module PI = Parse_info

open Entity_code open Highlight_code

module T = Parser_hs

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

let visit_program ~tag_hook _prefs  (_prog, toks) =

  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.add already_tagged ii true
  )
  in
  (* -------------------------------------------------------------------- *)
  (* ast phase 1 *)
  (* -------------------------------------------------------------------- *)

  (* -------------------------------------------------------------------- *)
  (* toks phase 1 *)
  (* -------------------------------------------------------------------- *)
  (*
   * note: all TCommentSpace are filtered in xs so easier to write
   * rules (but regular comments are kept as well as newlines).
   *)

  let rec aux_toks xs =
    match xs with
    | [] -> ()

    (* a little bit pad specific *)
    |   T.TComment(ii)
        ::T.TCommentNewline _ii2
        ::T.TComment(ii3)
        ::T.TCommentNewline ii4
        ::T.TComment(ii5)
        ::xs ->

        let s = PI.str_of_info ii in
        let s5 =  PI.str_of_info ii5 in
        (match () with
         | _ when s =~ ".*\\*\\*\\*\\*" && s5 =~ ".*\\*\\*\\*\\*" ->
             tag ii CommentEstet;
             tag ii5 CommentEstet;
             tag ii3 CommentSection1
         | _ when s =~ ".*------" && s5 =~ ".*------" ->
             tag ii CommentEstet;
             tag ii5 CommentEstet;
             tag ii3 CommentSection2
         | _ when s =~ ".*####" && s5 =~ ".*####" ->
             tag ii CommentEstet;
             tag ii5 CommentEstet;
             tag ii3 CommentSection0
         | _ ->
             ()
        );
        aux_toks (T.TComment ii3::T.TCommentNewline ii4::T.TComment ii5::xs)

    (* Poor's man semantic tagger. Infer if ident is a func, or variable,
     * based on the few tokens around and the column information.
    *)
    | T.TIdent (_s, ii1)::T.TSymbol ("::", _ii3)::xs
      when PI.col_of_info ii1 = 0 ->

        tag ii1 (Entity (Function, (Def2 NoUse)));
        aux_toks xs

    | (T.Ttype _ | T.Tdata _ | T.Tnewtype _)::T.TIdent (_s, ii1)::xs ->
        tag ii1 (Entity (Type, Def2 NoUse));
        aux_toks xs



    (* a few false positives, for instance local typed variable
     * and method definitions in type class
    *)
    | T.TIdent (_s, ii1)::T.TSymbol ("::", _ii3)::xs
      when PI.col_of_info ii1 > 0 ->

        tag ii1 (Entity (Field, (Def2 NoUse)));
        aux_toks xs

    | T.TIdent (_s, ii1)::xs
      when PI.col_of_info ii1 = 0 ->

        tag ii1 FunctionEquation;
        aux_toks xs

    | T.TIdent (_s, ii1)::T.TSymbol ("<-", _ii3)::xs  ->
        tag ii1 UseOfRef;
        aux_toks xs

    (* too many false positives, for instance records building
        | T.TIdent (s, ii1)::T.TSymbol ("=", ii3)::xs  ->
            tag ii1 (Local Def);
            aux_toks xs
    *)

    | T.TIdent (_s, ii1)::T.TSymbol ("@", _ii2)::xs  ->
        tag ii1 (Local Def);
        aux_toks xs

    | T.TSymbol ("\\", _ii1)::T.TIdent (_s, ii2)::xs  ->
        tag ii2 (Parameter Def);
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
    | T.TComment ii ->
        if not (Hashtbl.mem already_tagged ii)
        then
          (* a little bit syncweb specific *)
          let s = PI.str_of_info ii in
          if s =~ "(\\*[sex]:" (* yep, s e x are the syncweb markers *)
          then tag ii CommentSyncweb
          else tag ii Comment

    | T.TCommentNewline _ii | T.TCommentSpace _ii
      -> ()

    | T.TUnknown ii
      -> tag ii Error
    | T.EOF _ii
      -> ()

    | T.TString (_s,ii) ->
        tag ii String

    | T.TChar (_s,ii) ->
        tag ii String

    | T.TNumber (_s,ii) ->
        tag ii Number

    | T.TCBracket ii
    | T.TOBracket ii
      -> tag ii TypeVoid (* TODO *)

    | T.TCParen ii
    | T.TOParen ii
      -> tag ii Punctuation

    | T.TCBrace ii
    | T.TOBrace ii
      -> tag ii Punctuation

    | T.TComma ii
    | T.TSemiColon ii
    | T.TPipe ii
      -> tag ii Punctuation

    | T.TSymbol (s, ii)
      ->
        let kind =
          match s with
          | "::" | "->" | "<-" | "=" ->
              Punctuation
          | _ -> Operator
        in
        tag ii kind

    | T.Tdata  ii -> tag ii Keyword
    | T.Tnewtype  ii -> tag ii Keyword
    | T.Ttype  ii -> tag ii Keyword
    | T.Tclass  ii -> tag ii Keyword
    | T.Tinstance  ii -> tag ii Keyword
    | T.Tdefault  ii -> tag ii Keyword
    | T.Tderiving  ii -> tag ii Keyword

    | T.Tdo  ii -> tag ii KeywordLoop
    | T.Tif  ii -> tag ii KeywordConditional
    | T.Tthen  ii -> tag ii KeywordConditional
    | T.Telse  ii -> tag ii KeywordConditional
    | T.Tcase  ii -> tag ii KeywordConditional
    | T.Tof  ii -> tag ii KeywordConditional
    | T.Tmodule  ii -> tag ii KeywordModule
    | T.Timport  ii -> tag ii KeywordModule

    | T.Tlet  ii -> tag ii Keyword
    | T.Tin  ii -> tag ii Keyword
    | T.Twhere  ii -> tag ii Keyword

    | T.Tinfix  ii -> tag ii Keyword
    | T.Tinfixl  ii -> tag ii Keyword
    | T.Tinfixr  ii -> tag ii Keyword

    | T.Tqualified  ii
    | T.Tas  ii
    | T.Thiding  ii
      -> tag ii KeywordModule


    | T.TIdent (s, ii)
      ->
        (match s with
         | "unsafePerformIO" -> tag ii BadSmell
         | _ -> ()
        )

    | T.TUpperIdent (_s, ii)
      ->
        (* could be a type or a constructor *)
        tag ii TypeVoid


  );

  (* -------------------------------------------------------------------- *)
  (* ast phase 2 *)

  ()
