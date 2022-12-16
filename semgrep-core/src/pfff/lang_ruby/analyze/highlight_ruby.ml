(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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

open Highlight_code
module T = Parser_ruby
module E = Entity_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Syntax highlighting for Ruby code for codemap (and now also efuns)
*)

(*****************************************************************************)
(* Helpers when have global-analysis information *)
(*****************************************************************************)

(* we generate fake value here because the real one are computed in a
 * later phase in rewrite_categ_using_entities in pfff_visual.
*)
let _def2 = Def2 NoUse
let use2 = Use2 (NoInfoPlace, UniqueDef, MultiUse)

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

let visit_program ~tag_hook _prefs (_program, toks) =

  (* tagger wrappers *)
  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.replace already_tagged ii true
  )
  in
  let tag_name (_s, ii) categ =
    (* so treat the most specific in the enclosing code and then
     * do not fear to write very general case patterns later because
     * the specific will have priority over the general
     * (e.g., a Method use vs a Field use)
    *)
    if not (Hashtbl.mem already_tagged ii)
    then tag ii categ
  in
  let tag_if_not_tagged ii categ =
    if not (Hashtbl.mem already_tagged ii)
    then tag ii categ
  in

  (* -------------------------------------------------------------------- *)
  (* AST phase 1 *)
  (* -------------------------------------------------------------------- *)

  (* -------------------------------------------------------------------- *)
  (* tokens phase 1 (list of tokens) *)
  (* -------------------------------------------------------------------- *)

  (* -------------------------------------------------------------------- *)
  (* Tokens phase 2 (individual tokens) *)
  (* -------------------------------------------------------------------- *)

  toks |> List.iter (fun tok ->
    match tok with

    (* specials *)
    | T.T_SPACE _
    | T.T_EOF _ ->
        ()
    | T.T_UNKNOWN ii ->
        tag ii Error

    (* comments *)
    (* in lexer_ruby.mll comments and space and newlines are sometimes
     * put together *)
    | T.T_COMMENT ii
    | T.T_EOL ii ->
        tag_if_not_tagged ii Comment

    (* values  *)
    | T.K_TRUE ii | T.K_FALSE ii ->
        tag ii Boolean
    | T.T_FLOAT (_, ii) | T.T_NUM (_, ii) ->
        tag ii Number
    | T.K_NIL ii ->
        tag ii Null
    | T.T_ATOM (ii1, (_, ii2)) ->
        tag ii1 Atom; tag ii2 Atom
    | T.T_ATOM_BEG ii ->
        tag ii Atom
    | T.T_SINGLE_STRING (_, ii)
    | T.T_INTERP_STR (_, ii)
    | T.T_INTERP_END (_, ii)
    | T.T_DOUBLE_BEG ii
    | T.T_USER_BEG (_, ii)
      -> tag ii String

    | T.T_REGEXP_BEG ii ->
        tag ii Regexp
    | T.T_REGEXP_MOD (_, ii) ->
        tag ii Regexp

    (* ident  *)
    | T.T_LID (s, ii) | T.T_UID (s, ii) ->
        tag_name (s, ii) Normal

    | T.T_CLASS_VAR (_, ii) ->
        tag ii (Entity (E.Global, use2))
    | T.T_INST_VAR (_, ii) ->
        tag ii (Entity (E.Field, use2))
    | T.T_GLOBAL_VAR (_, ii) ->
        tag ii (Entity (E.Global, use2))

    (* keywords  *)
    | T.K_DEF ii | T.K_ALIAS ii ->
        tag ii Keyword
    | T.K_lBEGIN ii | T.K_lEND ii | T.K_RETURN ii ->
        tag ii Keyword
    | T.K_IF ii | T.K_ELSE ii | T.K_ELSIF ii | T.K_THEN ii | T.K_UNLESS ii
    | T.K_WHEN ii | T.K_CASE ii
      -> tag ii KeywordConditional
    | T.K_FOR ii | T.K_UNTIL ii | T.K_WHILE ii ->
        tag ii KeywordLoop
    | T.K_ENSURE ii | T.K_RESCUE ii ->
        tag ii KeywordExn
    | T.K_SELF ii | T.K_SUPER ii | T.K_CLASS ii ->
        tag ii KeywordObject
    | T.K_MODULE ii ->
        tag ii KeywordModule
    | T.K_YIELD ii ->
        tag ii KeywordConcurrency

    | T.K_AND ii | T.K_OR ii | T.K_NOT ii ->
        tag ii BuiltinBoolean

    | T.K_DO ii
    | T.K_IN ii
      -> tag ii Keyword

    | T.K_BEGIN ii
    | T.K_END ii
      -> tag ii BadSmell

    | T.K_UNDEF ii
      -> tag ii BadSmell

    | T.T_TICK_BEG ii
      -> tag ii BadSmell

    (* symbols *)
    | T.T_PLUS ii | T.T_UPLUS ii
    | T.T_MINUS ii | T.T_UMINUS ii
    | T.T_STAR ii | T.T_USTAR ii
    | T.T_SLASH ii
    | T.T_PERCENT ii
    | T.T_POW ii
    | T.T_LSHFT ii | T.T_RSHFT ii

    | T.T_EQ ii | T.T_ASSIGN ii
    | T.T_CMP ii
    | T.T_LT ii | T.T_GT ii
    | T.T_LEQ ii | T.T_GEQ ii
    | T.T_NEQ ii | T.T_EQQ ii
    | T.T_OROP ii | T.T_ANDOP ii
    | T.T_MATCH ii | T.T_NMATCH ii

    | T.T_AMPER ii | T.T_UAMPER ii
    | T.T_VBAR ii
    | T.T_CARROT ii

    | T.T_TILDE ii
    | T.T_BANG ii
      -> tag ii Operator

    | T.T_COLON ii
    | T.T_QUESTION ii

    | T.T_RBRACE ii
    | T.T_LBRACE ii | T.T_LBRACE_ARG ii
    | T.T_RBRACK ii
    | T.T_LBRACK ii | T.T_LBRACK_ARG ii
    | T.T_RPAREN ii
    | T.T_LPAREN ii | T.T_LPAREN_ARG ii
      -> tag ii Punctuation

    | T.T_RARROW ii
    | T.T_ASSOC ii
    | T.T_COMMA ii
    | T.T_DOT ii
    | T.T_SEMICOLON ii
    | T.T_SCOPE ii
    | T.T_USCOPE ii
    | T.T_DOT3 ii
    | T.T_DOT2 ii
    | T.T_OP_ASGN (_, ii)
    | T.LDots ii | T.RDots ii
      -> tag ii Punctuation
  );
  ()
