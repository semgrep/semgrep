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
module PI = Parse_info
module T = Parser_lisp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

let visit_toplevel ~tag_hook _prefs  (_toplevel, toks) =

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
    | T.TOParen ii1::T.TIdent (s2, _ii2)::T.TIdent (_s3, ii3)::xs
      when PI.col_of_info ii1 = 0 ->
        (match s2 with
         | "setq" | "defvar" ->
             tag ii3 (Entity (Global, (Def2 NoUse)))
         | "defun" ->
             tag ii3 (Entity (Function, (Def2 NoUse)))
         | "defconst" ->
             tag ii3 (Entity (Constant, (Def2 NoUse)))
         | _ -> ()
        );
        aux_toks xs

    (* scheme/racket *)
    | T.TOParen ii1::T.TIdent (s2, _ii2)
      ::T.TOParen _iibis::T.TIdent (_s3, ii3)::xs
      when PI.col_of_info ii1 = 0 ->
        (match s2 with
         | "define" ->
             tag ii3 (Entity (Function, (Def2 NoUse)))
         | _ -> ()
        );
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

    | T.TNumber (_s,ii) ->
        tag ii Number

    | T.TIdent (s, ii) ->
        (match s with
         (* lisp *)
         | "defun"
           -> tag ii Keyword
         | "setq"  | "defvar"
           -> tag ii KeywordObject (* hmm not really *)

         (* scheme *)
         | "define"
           -> tag ii Keyword


         | "let"  | "let*"
           -> tag ii KeywordObject

         | "defconst"
           -> tag ii KeywordObject

         | "require" | "provide"
         | "module" (* racket *)
           -> tag ii KeywordModule

         | "t"  ->
             tag ii Boolean
         | "nil"  ->
             tag ii Boolean (* or Null ? *)

         | "cond" | "if" ->
             tag ii KeywordConditional
         | "while" | "do" ->
             tag ii KeywordLoop

         | "concat"
         | "getenv"
           ->  tag ii Builtin

         | _ -> ()
        )


    | T.TCBracket ii
    | T.TOBracket ii
      -> tag ii TypeVoid (* TODO *)

    | T.TCParen ii
    | T.TOParen ii
      -> tag ii Punctuation

    | T.TQuote ii ->
        tag ii EmbededHtml (* quote stuff is kind of like XHP after all *)

    | T.TAt ii
    | T.TComma ii
    | T.TBackQuote ii
      ->
        tag ii EmbededHtml (* quote stuff is kind of like XHP after all *)



  );

  (* -------------------------------------------------------------------- *)
  (* ast phase 2 *)

  ()
