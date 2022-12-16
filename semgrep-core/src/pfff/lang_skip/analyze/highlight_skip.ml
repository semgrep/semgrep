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
open Common

open Highlight_code
module E = Entity_code
module T = Parser_skip
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Syntax highlighting for Skip code for codemap (and now also for efuns).
 *
*)

(*****************************************************************************)
(* Helpers when have global analysis information *)
(*****************************************************************************)

let def2 = Def2 NoUse
let use2 = Use2 (NoInfoPlace, UniqueDef, MultiUse)

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

(* The idea of the code below is to visit the program either through its
 * AST or its list of tokens. The tokens are easier for tagging keywords,
 * number and basic entities. The AST is better for tagging idents
 * to figure out what kind of ident it is.
*)
let visit_program ~tag_hook _prefs  (_astopt, toks) =

  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.add already_tagged ii true
  )
  in
  let tag_if_not_tagged ii categ =
    if not (Hashtbl.mem already_tagged ii)
    then tag ii categ
  in

  let lexer_based_tagger = true in
  let tag_if_lexer ii categ =
    if not (Hashtbl.mem already_tagged ii) && lexer_based_tagger
    then tag ii categ
  in

  (* -------------------------------------------------------------------- *)
  (* AST phase 1 *)
  (* -------------------------------------------------------------------- *)

  (* -------------------------------------------------------------------- *)
  (* Tokens phase 1 (sequence of tokens) *)
  (* -------------------------------------------------------------------- *)
  (* note: all TCommentSpace are filtered in xs so it should be easier to
   * write rules (but regular comments are kept as well as newlines).
  *)
  let rec aux_toks xs =
    match xs with
    | [] -> ()

    (* pad-specific: *)
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

    |   T.TComment(ii)::xs when (PI.str_of_info ii) =~ "(\\*[ ]*coupling:" ->
        tag ii CommentImportance3;
        aux_toks xs


    (* When we get a parse error, the AST does not contain the definitions, but
     * we can still try to tag certain things. Here is a
     * poor's man semantic tagger. We try to infer if an ident is a func,
     * or class, or module based on the few tokens around.
     *
     * This may look ridiculous to do such semantic tagging using tokens
     * instead of the full AST but sometimes Skip files could not parse with
     * the default parser because of ??? so having
     * a solid token-based tagger is still useful as a last resort.
    *)

    (* Entity definitions *)

    | T.Tconst(_ii)::T.TLowerIdent(_s, ii3)::xs ->
        tag_if_lexer ii3 (Entity (E.Constant, def2));
        aux_toks xs;
    | T.Tfun(_ii)::T.TLowerIdent(_s, ii3)::xs ->
        tag_if_lexer ii3 (Entity (E.Function, def2));
        aux_toks xs;
    | T.Tclass(_ii)::T.TUpperIdent(_s, ii3)::xs ->
        tag_if_lexer ii3 (Entity (E.Class, def2));
        aux_toks xs;
    | T.Tclass(_ii)::T.TDot _::T.TUpperIdent(_s, ii3)::xs ->
        tag_if_lexer ii3 (Entity (E.Class, def2));
        aux_toks xs;
    | T.Ttrait(_ii)::T.TUpperIdent(_s, ii3)::xs ->
        tag_if_lexer ii3 (Entity (E.Class, def2));
        aux_toks xs;
    | T.Ttype(_ii)::T.TUpperIdent(_s, ii3)::xs ->
        tag_if_lexer ii3 (Entity (E.Type, def2));
        aux_toks xs;
    | T.Tmodule(_ii)::T.TUpperIdent(_s, ii3)::xs ->
        tag_if_lexer ii3 (Entity (E.Module, def2));
        aux_toks xs;
    | T.Tmodule _::T.Talias _::T.TUpperIdent(_s, ii3)::xs ->
        tag_if_lexer ii3 (Entity (E.Module, def2));
        aux_toks xs;

        (* module use, and function call! *)
    | T.TUpperIdent(_, ii)::T.TDot _::T.TLowerIdent(_, ii2)::T.TOParen _::xs ->
        tag_if_lexer ii (Entity (E.Module, use2));
        tag_if_lexer ii2 (Entity (E.Function, use2));
        aux_toks xs;
    | T.TLowerIdent(_, _ii)::T.TDot _::T.TLowerIdent(_, ii2)::T.TOParen _::xs ->
        (*tag_if_lexer ii (Entity (E.Module, use2));*)
        tag_if_lexer ii2 (Entity (E.Method, use2));
        aux_toks xs;

    | T.TCParen(_)::T.TDot _::T.TLowerIdent(_, ii2)::T.TOParen _::xs ->
        (*tag_if_lexer ii (Entity (E.Module, use2));*)
        tag_if_lexer ii2 (Entity (E.Method, use2));
        aux_toks xs;

    | T.TLowerIdent(_, ii)::T.TOParen _::xs ->
        tag_if_lexer ii (Entity (E.Function, use2));
        aux_toks xs;

    | T.TUpperIdent(_, ii)::T.TDot _::T.TLowerIdent(_, _ii2)::xs ->
        tag_if_lexer ii (Entity (E.Module, use2));
        aux_toks xs;
    | T.TUpperIdent(_, ii)::T.TDot _::T.TUpperIdent(_, _ii2)::xs ->
        tag_if_lexer ii (Entity (E.Module, use2));
        aux_toks xs;
    | T.TLowerIdent(_, _ii)::T.TDot _::T.TLowerIdent(_, ii2)::xs ->
        tag_if_lexer ii2 (Entity (E.Field, use2));
        aux_toks xs;

    | T.TLowerIdent(_, ii)::T.TArrow _::xs ->
        tag_if_lexer ii (Parameter Def);
        aux_toks xs;

    | T.TLowerIdent(_, ii)::T.TEqualArrow _::xs ->
        tag_if_lexer ii (Entity (E.Field, use2));
        aux_toks xs;

        (* recurse *)
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
  (* Tokens phase 2 (individual tokens) *)
  (* -------------------------------------------------------------------- *)

  toks |> List.iter (fun tok ->
    match tok with
    (* specials *)

    | T.TComment ii ->
        (* a little bit syncweb specific *)
        let s = PI.str_of_info ii in
        (match s with
         (* yep, s e x are the syncweb markers *)
         | _ when s =~ "(\\*[sex]:"  -> tag ii CommentSyncweb
         (* normally then use of *** or ### or --- should be enough,
          * but in some files like ocamlyacc files the preceding
          * heuristic fail in which case it's useful to have those
          * rules. Moreover ocamldoc use something similar
         *)
         | _ when s =~ "(\\*1 "  -> tag ii CommentSection1
         | _ when s =~ "(\\*2 "  -> tag ii CommentSection2
         | _ when s =~ "(\\*3 "  -> tag ii CommentSection3
         | _ -> tag_if_not_tagged ii Comment
        )
    | T.TCommentNewline _ii | T.TCommentSpace _ii -> ()
    | T.TUnknown ii -> tag ii Error
    | T.EOF _ii-> ()

    (* values *)

    | T.Tfalse ii | T.Ttrue ii -> tag ii Boolean
    | T.TFloat (_s,ii) | T.TInt (_s,ii) -> tag ii Number
    | T.TChar (_s, ii) -> tag ii String
    | T.TString (_s,ii) -> tag ii String

    (* keywords *)

    | T.Tif ii | T.Telse ii | T.Tmatch ii
      -> tag ii KeywordConditional
    | T.Ttry ii | T.Tcatch ii | T.Tthrow ii
      -> tag ii KeywordExn
    | T.Tfor ii | T.Tin ii | T.Twhile ii | T.Tloop ii | T.Tdo ii
      -> tag ii KeywordLoop
    | T.Tfrom ii
      -> tag ii Keyword
    | T.Tmodule ii | T.Talias ii
      -> tag ii KeywordModule
    | T.Tclass ii | T.Textends ii | T.Tuses ii

    | T.Ttrait ii
    | T.Tthis ii
    | T.Tchildren ii
    | T.Textension ii
      -> tag ii KeywordObject
    | T.Tfun ii | T.Ttype ii | T.Tconst ii
    | T.Tasync ii | T.Tawait ii
    | T.Tyield ii
    | T.Tbreak ii | T.Tcontinue ii
      -> tag ii Keyword
    | T.Tvoid ii
      -> tag ii TypeVoid

    | T.Tfinal ii
    | T.Tmutable ii
    | T.Tnative ii
    | T.Tstatic ii
    | T.Tprivate ii | T.Tprotected ii
    | T.Toverridable ii
    | T.Treadonly ii
      -> tag ii Attribute

    | T.Tmacro ii -> tag ii CppOther

    | T.Tas ii
    | T.Twatch ii
    | T.Twhen ii
    | T.Twith ii
      -> tag ii Keyword

    (* conditional keywords *)
    | T.Tbase ii -> tag ii KeywordObject
    | T.Tinst ii -> tag ii TypeVoid
    | T.Tcapture ii
    | T.Tdefault ii
    | T.Tdeferred ii
    | T.TnonNullable ii
    | T.Tuntracked ii
      -> tag ii Keyword

    | T.Tvalue _ ->
        ()

    (* not in original spec *)
    | T.Tmemoized ii
    | T.Tfrozen ii
      -> tag ii Attribute

    (* Punctuation *)

    | T.TBang ii ->
        tag ii UseOfRef

    | T.TEq ii ->
        tag ii Punctuation

    | T.TSemiColon ii | T.TPipe ii | T.TComma ii
    | T.TOBracket ii | T.TCBracket ii
    | T.TOBrace ii | T.TCBrace ii
    | T.TOParen ii | T.TCParen ii

    | T.TDot ii
    | T.TColon ii

    | T.TSemiColonSemiColon ii

    | T.TEqDot ii
    | T.TTildeArrow ii
    | T.TEqualArrow ii
    | T.TEqEq ii
    | T.TLessEq ii
    | T.TGreaterEq ii
    | T.THat ii
    | T.TPipePipe ii

      -> tag ii Punctuation

    (* Operators *)

    | T.TPlus ii | T.TMinus ii
    | T.TStar ii | T.TDiv ii | T.TMod ii
    | T.TLess ii | T.TGreater ii

    | T.TColonColon ii
    | T.TAnd ii | T.TAndAnd ii
    | T.TArrow ii | T.TBangEq ii
    | T.TQuestion ii
    | T.TAt ii
    | T.TSharp ii
    | T.TDollar ii
    | T.TBackquote ii

      -> tag ii Punctuation

    (* Idents *)

    | T.TLowerIdent (s, _ii) ->
        (match s with
(*
        | _ when Hashtbl.mem h_pervasives_pad s ->
            tag ii BuiltinCommentColor
        | _ when Hashtbl.mem h_builtin_bool s ->
            tag ii BuiltinBoolean
        | "failwith" | "raise" ->
            tag ii KeywordExn
*)
         | _ ->
             ()
        )

    | T.TUpperIdent (_s, ii) ->
        let kind = E.Type in
        tag_if_not_tagged ii (Entity (kind, use2))

  )
