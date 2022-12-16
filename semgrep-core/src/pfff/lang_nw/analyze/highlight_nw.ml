(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2015, 2018 Yoann Padioleau
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

module T = Lexer_nw
module TH = Token_helpers_nw
module F = Ast_fuzzy
module LF = Lib_ast_fuzzy

open Highlight_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let span_newline xs = xs |> Common2.split_when (function
  | T.TCommentNewline _ -> true | _ -> false)

let tag_all_tok_with ~tag categ xs =
  xs |> List.iter (fun tok ->
    let info = TH.info_of_tok tok in
    tag info categ
  )

let tag_all_tok_trees_with ~tag categ trees =
  let xs = Lib_ast_fuzzy.toks_of_trees trees in
  xs |> List.iter (fun info -> tag info categ)

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

(* The idea of the code below is to visit the program either through its
 * AST or its list of tokens. The tokens are easier for tagging keywords,
 * number and basic entities. The AST is better for other things.
*)
let visit_program ~tag_hook _prefs (trees, toks) =
  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.replace already_tagged ii true
  )
  in

  (* -------------------------------------------------------------------- *)
  (* toks phase 1 (sequence of tokens) *)
  (* -------------------------------------------------------------------- *)

  let rec aux_toks xs =
    match xs with
    | [] -> ()

    (* pad-specific: *)
    |   T.TComment(ii)
        ::T.TCommentNewline _ii2
        ::T.TComment(ii3)
        ::T.TCommentNewline _ii4
        ::T.TComment(ii5)
        ::xs ->
        let s = Parse_info.str_of_info ii in
        let s5 =  Parse_info.str_of_info ii5 in
        (match () with
         | _ when s =~ "^%\\*\\*\\*\\*" && s5 =~ "^%\\*\\*\\*\\*" ->
             tag ii CommentEstet; tag ii5 CommentEstet;
             tag ii3 CommentSection0
         | _ when s =~ "^%------" && s5 =~ "^%------" ->
             tag ii CommentEstet; tag ii5 CommentEstet;
             tag ii3 CommentSection1
         | _ when s =~ "^%####" && s5 =~ "^%####" ->
             tag ii CommentEstet; tag ii5 CommentEstet;
             tag ii3 CommentSection2
         | _ ->
             ()
        );
        aux_toks xs

    (* syncweb-specific: *)
    | T.TSymbol("#", _ii)::T.TWord("include", ii2)::xs ->
        tag ii2 Include;
        aux_toks xs

    (* specific to texinfo *)
    | T.TSymbol("@", _)::T.TWord(s, ii)::xs ->
        let categ_opt =
          (match s with
           | "title" -> Some CommentSection0
           | "chapter" -> Some CommentSection0
           | "section" -> Some CommentSection1
           | "subsection" -> Some CommentSection2
           | "subsubsection" -> Some CommentSection3
           | "c" -> Some Comment
           (* don't want to polluate my view with indexing "aspect" *)
           | "cindex" ->
               tag ii Comment;
               Some Comment
           | _ -> None
          )
        in
        (match categ_opt with
         | None ->
             tag ii Keyword;
             aux_toks xs
         | Some categ ->
             let (before, _, _) = span_newline xs in
             tag_all_tok_with ~tag categ before;
             (* repass on tokens, in case there are nested tex commands *)
             aux_toks xs
        )

    (* specific to web TeX source: ex: @* \[24] Getting the next token. *)
    |    T.TSymbol("@*", _)
         :: T.TCommentSpace _
         :: T.TSymbol("\\", _)
         :: T.TSymbol("[", ii1)
         :: T.TNumber(_, iinum)
         :: T.TSymbol("]", ii2)
         :: T.TCommentSpace _
         :: xs
      ->
        let (before, _, _) = span_newline xs in
        [ii1;iinum;ii2] |> List.iter (fun ii -> tag ii CommentSection0);
        tag_all_tok_with ~tag CommentSection0 before;
        (* repass on tokens, in case there are nested tex commands *)
        aux_toks xs
    (* less: \item TWord => purple? or maybe purple until end of line *)

    | _x::xs ->
        aux_toks xs
  in
  let toks' = toks |> Common.exclude (function
    (* needed ? *)
    (* | T.TCommentSpace _ -> true *)
    | _ -> false
  )
  in
  aux_toks toks';

  (* -------------------------------------------------------------------- *)
  (* AST phase 1 *)
  (* -------------------------------------------------------------------- *)
  trees |> LF.mk_visitor { LF.default_visitor with
                           LF.ktrees = (fun (k, _v) trees ->
                             match trees with
                             (* \xxx{...} *)
                             | F.Tok (s, _)::F.Braces (_, brace_trees, _)::_ ->
                                 let categ_opt =
                                   match s with

                                   | ("\\chapter" | "\\chapter*") -> Some CommentSection0
                                   | "\\section" -> Some CommentSection1
                                   | "\\subsection" -> Some CommentSection2
                                   | "\\subsubsection" -> Some CommentSection3

                                   | "\\label" -> Some (Label Def)
                                   | "\\ref" -> Some (Label Def)
                                   | "\\cite" -> Some (Label Def)
                                   (* principia-specific: *)
                                   | "\\book" -> Some (Label Def)

                                   | "\\begin" | "\\end" -> Some KeywordExn (* TODO *)
                                   | "\\input" | "\\usepackage" | "\\bibliography" ->
                                       Some IncludeFilePath
                                   | "\\url" | "\\furl" -> Some EmbededUrl

                                   | _ when s =~ "^\\" -> Some (Parameter Use)
                                   | _ -> None
                                 in
                                 categ_opt |> Option.iter (fun categ ->
                                   tag_all_tok_trees_with ~tag categ brace_trees;
                                 );
                                 k trees

                             (* \xxx[...]{...} *)
                             | F.Tok (s, _)::F.Bracket(_,params,_)::F.Braces (_,body, _)::_ ->
                                 if s =~ "^\\" then begin
                                   tag_all_tok_trees_with ~tag (Parameter Use) params;
                                   tag_all_tok_trees_with ~tag (Parameter Use) body;
                                 end;
                                 k trees
                             (* {...}{...} *)
                             | F.Braces (_, brace_trees1,_)::F.Braces (_, brace_trees2,_)::_ ->
                                 tag_all_tok_trees_with ~tag (Parameter Use) brace_trees1;
                                 tag_all_tok_trees_with ~tag (Parameter Use) brace_trees2;
                                 k trees

                             (* {\xxx ... } *)
                             | F.Braces (_, (F.Tok (s, _)::brace_trees),_)::_ ->
                                 let categ_opt =
                                   match s with
                                   | "\\em" -> Some CommentWordImportantNotion
                                   | _ -> None
                                 in
                                 categ_opt |> Option.iter (fun categ ->
                                   tag_all_tok_trees_with ~tag categ brace_trees;
                                 );
                                 k trees

                             | _ -> k trees
                           );
                         };

  (* -------------------------------------------------------------------- *)
  (* toks phase 2 (individual tokens) *)
  (* -------------------------------------------------------------------- *)

  toks |> List.iter (fun tok ->
    match tok with
    | T.TComment ii ->
        if not (Hashtbl.mem already_tagged ii)
        then
          let s = Parse_info.str_of_info ii |> String.lowercase_ascii in
          (match s with
           | _ when s =~ "^%todo:" -> tag ii BadSmell
           | _ -> tag ii CommentImportance0
          )
    | T.TCommentSpace _ii -> ()
    | T.TCommentNewline _ii -> ()

    | T.TCommand (s, ii) ->
        let categ =
          (match s with
           | s when s =~ "^if" -> KeywordConditional
           | s when s =~ ".*true$" -> Boolean
           | s when s =~ ".*false$" -> Boolean

           | "fi" -> KeywordConditional
           | "input" | "usepackage" -> Include
           | "appendix" -> CommentSection0

           | _ -> Keyword
          )
        in
        tag ii categ

    | T.TWord (s, ii) ->
        (match s with
         | "TODO" -> tag ii BadSmell
         | _ -> ()
        )

    (* noweb-specific: (obviously) *)
    | T.TBeginNowebChunk ii
    | T.TEndNowebChunk ii
      ->
        tag ii KeywordExn (* TODO *)

    | T.TNowebChunkStr (_, ii) ->
        tag ii EmbededCode
    | T.TNowebCode (_, ii) ->
        tag ii EmbededCode
    | T.TNowebCodeLink (_, ii) ->
        tag ii (Label Def) (* TODO *)

    | T.TNowebChunkName (_, ii) ->
        tag ii KeywordObject (* TODO *)

    | T.TBeginVerbatim ii | T.TEndVerbatim ii -> tag ii Keyword

    | T.TVerbatimLine (_, ii) ->
        tag ii Verbatim

    (* syncweb-specific: *)
    | T.TFootnote (c, ii) ->
        (match c with
         | 't' -> tag ii BadSmell
         | 'n' -> tag ii Comment
         | 'l' -> tag ii CommentImportance1
         | _ -> failwith (spf "syncweb \\x special macro not recognized:%c" c)
        )

    | T.TNumber (_, ii) | T.TUnit (_, ii) ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii Number


    | T.TSymbol (s, ii) ->
        (match s with
         | "&" | "\\\\" ->
             tag ii Punctuation
         | _ -> ()
        )

    | T.TOBrace ii | T.TCBrace ii
    | T.TOBracket ii | T.TCBracket ii
      ->  tag ii Punctuation

    | T.TUnknown ii -> tag ii Error
    | T.EOF _ii-> ()

  );

  (* -------------------------------------------------------------------- *)
  (* AST phase 2 *)

  ()
