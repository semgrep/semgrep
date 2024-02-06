(* Brandon Wu
 *
 * Copyright (C) 2024 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

module H = Parse_tree_sitter_helpers
module CST = Tree_sitter_cpp.CST

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This file is for preprocessor-related boilerplate. *)

(* The primary motivating factor is that there is a function preprocIf
   (https://github.com/tree-sitter/tree-sitter-c/blob/ecdd500806cf8154d944344f1df6418b32e0e9a7/grammar.js#L1328)
   which generates tree-sitter nonterminals that all look very similar, but each
   different type stores a different type of data, essentially similar to an
   'a tree.

   Unfortunately, the generated CSTs have no idea about this similar structure,
   and all register as different types. This means that although each use site
   of preprocIf generates trees with similar structure, we cannot use the same
   code to translate it, as the trees have different types.

   So we end up with types like preproc_if, preproc_if_in_field_declaration,
   preproc_if_in_enumerator_list, which all look like concrete instances of a
   common polymorphic type.

   To prevent code duplication (and maintenance burden), we will maintain this
   `preproc_if_poly` type here (and friends), which encode the similar structure
   of all these types. Our strategy will be to first translate each concrete
   `preproc_if` to this `preproc_if_poly` type, and then translate the common
   `preproc_if_poly` to the AST.

   This means that we only need to keep one copy of the actual translation logic.
*)

(* coupling: Note that this file is identical to `Preprocessor_c.ml`.
   This is regrettable, because this is also a measure of code duplication, but
   we can't reuse these types between both, because the type of
   `preproc_expression` used in both are different, and are too deep to use the
   same trick as we do here.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type ifdef_token =
  [ `Ifdef of Tree_sitter_run.Token.t (* pattern #[ 	]*ifdef *)
  | `Ifndef of Tree_sitter_run.Token.t (* pattern #[ 	]*ifndef *) ]

type elifdef_token =
  [ `Elifdef of Tree_sitter_run.Token.t (* pattern #[ 	]*elifdef *)
  | `Elifndef of Tree_sitter_run.Token.t (* pattern #[ 	]*elifndef *) ]

type 'a preproc_if_poly =
  Tree_sitter_run.Token.t (* pattern #[ 	]*if *)
  * CST.preproc_expression
  * Tree_sitter_run.Token.t (* "\n" *)
  * 'a list (* zero or more *)
  * 'a preproc_else_poly option
  * Tree_sitter_run.Token.t (* pattern #[ 	]*endif *)

and 'a preproc_else_poly =
  [ `Prep_else_poly of
    Tree_sitter_run.Token.t (* pattern #[ 	]*else *) * 'a list
  | `Prep_elif_poly of
    Tree_sitter_run.Token.t (* pattern #[ 	]*elif *)
    * CST.preproc_expression
    * Tree_sitter_run.Token.t (* "\n" *)
    * 'a list (* zero or more *)
    * 'a preproc_else_poly option ]

type 'a preproc_ifdef_poly =
  ifdef_token
  * Tree_sitter_run.Token.t (* identifier *)
  * 'a list (* zero or more *)
  * [ `Choice_prep_else_poly of 'a preproc_else_poly
    | `Prep_elif_poly of 'a preproc_elifdef_poly ]
    option
  * Tree_sitter_run.Token.t (* pattern #[ 	]*endif *)

and 'a preproc_elifdef_poly =
  elifdef_token
  * Tree_sitter_run.Token.t (* identifier *)
  * 'a list (* zero or more *)
  * 'a preproc_else_poly option

(*****************************************************************************)
(* translation to polymorphic type *)
(*****************************************************************************)

let elifdef_token_to_poly (x : CST.anon_choice_pat_0307ca2_dbf6a9d) :
    elifdef_token =
  match x with
  | `Pat_0307ca2 x -> `Elifdef x
  | `Pat_a6d4183 x -> `Elifndef x

let ifdef_token_to_poly (x : CST.anon_choice_pat_25b90ba_4a37f8c) =
  match x with
  | `Pat_25b90ba x -> `Ifdef x
  | `Pat_9d92f6a x -> `Ifndef x

(* preprocIf(block_item) *)
let rec choice_to_poly x =
  match x with
  | `Choice_prep_else x -> `Choice_prep_else_poly (preproc_else_to_poly x)
  | `Prep_elif x -> `Prep_elif_poly (preproc_elifdef_to_poly x)

and preproc_elifdef_to_poly ((v1, v2, v3, v4) : CST.preproc_elifdef) :
    CST.block_item preproc_elifdef_poly =
  (elifdef_token_to_poly v1, v2, v3, Option.map preproc_else_to_poly v4)

and preproc_if_to_poly ((v1, v2, v3, v4, v5, v6) : CST.preproc_if) :
    CST.block_item preproc_if_poly =
  (v1, v2, v3, v4, Option.map preproc_else_to_poly v5, v6)

and preproc_ifdef_to_poly ((v1, v2, v3, v4, v5) : CST.preproc_ifdef) :
    CST.block_item preproc_ifdef_poly =
  (ifdef_token_to_poly v1, v2, v3, Option.map choice_to_poly v4, v5)

and preproc_else_to_poly (x : CST.anon_choice_prep_else_8b52b0f) :
    CST.block_item preproc_else_poly =
  match x with
  | `Prep_else (v1, v2) -> `Prep_else_poly (v1, v2)
  | `Prep_elif (v1, v2, v3, v4, v5) ->
      `Prep_elif_poly (v1, v2, v3, v4, Option.map preproc_else_to_poly v5)

(* preprocIf(field_declaration_list_to_poly_item) *)
let rec choice_in_field_declaration_list_to_poly x =
  match x with
  | `Choice_prep_else_in_field_decl_list x ->
      `Choice_prep_else_poly (preproc_else_in_field_declaration_list_to_poly x)
  | `Prep_elif_in_field_decl_list x ->
      `Prep_elif_poly (preproc_elifdef_in_field_declaration_list_to_poly x)

and preproc_elifdef_in_field_declaration_list_to_poly
    ((v1, v2, v3, v4) : CST.preproc_elifdef_in_field_declaration_list) :
    CST.field_declaration_list_item preproc_elifdef_poly =
  ( elifdef_token_to_poly v1,
    v2,
    v3,
    Option.map preproc_else_in_field_declaration_list_to_poly v4 )

and preproc_if_in_field_declaration_list_to_poly
    ((v1, v2, v3, v4, v5, v6) : CST.preproc_if_in_field_declaration_list) :
    CST.field_declaration_list_item preproc_if_poly =
  ( v1,
    v2,
    v3,
    v4,
    Option.map preproc_else_in_field_declaration_list_to_poly v5,
    v6 )

and preproc_ifdef_in_field_declaration_list_to_poly
    ((v1, v2, v3, v4, v5) : CST.preproc_ifdef_in_field_declaration_list) :
    CST.field_declaration_list_item preproc_ifdef_poly =
  ( ifdef_token_to_poly v1,
    v2,
    v3,
    Option.map choice_in_field_declaration_list_to_poly v4,
    v5 )

and preproc_else_in_field_declaration_list_to_poly
    (x : CST.anon_choice_prep_else_in_field_decl_list_97ea65e) :
    CST.field_declaration_list_item preproc_else_poly =
  match x with
  | `Prep_else_in_field_decl_list (v1, v2) -> `Prep_else_poly (v1, v2)
  | `Prep_elif_in_field_decl_list (v1, v2, v3, v4, v5) ->
      `Prep_elif_poly
        ( v1,
          v2,
          v3,
          v4,
          Option.map preproc_else_in_field_declaration_list_to_poly v5 )

(* preprocIf(seq(enumerator, ",")) *)
let rec choice_in_enumerator_list_to_poly x =
  match x with
  | `Choice_prep_else_in_enum_list x ->
      `Choice_prep_else_poly (preproc_else_in_enumerator_list_to_poly x)
  | `Prep_elif_in_enum_list x ->
      `Prep_elif_poly (preproc_elifdef_in_enumerator_list_to_poly x)

and preproc_elifdef_in_enumerator_list_to_poly
    ((v1, v2, v3, v4) : CST.preproc_elifdef_in_enumerator_list) :
    (CST.enumerator * Tree_sitter_run.Token.t) preproc_elifdef_poly =
  ( elifdef_token_to_poly v1,
    v2,
    v3,
    Option.map preproc_else_in_enumerator_list_to_poly v4 )

and preproc_if_in_enumerator_list_to_poly
    ((v1, v2, v3, v4, v5, v6) : CST.preproc_if_in_enumerator_list) :
    (CST.enumerator * Tree_sitter_run.Token.t) preproc_if_poly =
  (v1, v2, v3, v4, Option.map preproc_else_in_enumerator_list_to_poly v5, v6)

and preproc_ifdef_in_enumerator_list_to_poly
    ((v1, v2, v3, v4, v5) : CST.preproc_ifdef_in_enumerator_list) :
    (CST.enumerator * Tree_sitter_run.Token.t) preproc_ifdef_poly =
  ( ifdef_token_to_poly v1,
    v2,
    v3,
    Option.map choice_in_enumerator_list_to_poly v4,
    v5 )

and preproc_else_in_enumerator_list_to_poly
    (x : CST.anon_choice_prep_else_in_enum_list_8258275) :
    (CST.enumerator * Tree_sitter_run.Token.t) preproc_else_poly =
  match x with
  | `Prep_else_in_enum_list (v1, v2) -> `Prep_else_poly (v1, v2)
  | `Prep_elif_in_enum_list (v1, v2, v3, v4, v5) ->
      `Prep_elif_poly
        (v1, v2, v3, v4, Option.map preproc_else_in_enumerator_list_to_poly v5)

(* preprocIf(enumerator) *)
let rec choice_in_enumerator_list_no_comma_to_poly x =
  match x with
  | `Choice_prep_else_in_enum_list_no_comma x ->
      `Choice_prep_else_poly
        (preproc_else_in_enumerator_list_no_comma_to_poly x)
  | `Prep_elif_in_enum_list_no_comma x ->
      `Prep_elif_poly (preproc_elifdef_in_enumerator_list_no_comma_to_poly x)

and preproc_elifdef_in_enumerator_list_no_comma_to_poly
    ((v1, v2, v3, v4) : CST.preproc_elifdef_in_enumerator_list_no_comma) :
    CST.enumerator preproc_elifdef_poly =
  ( elifdef_token_to_poly v1,
    v2,
    v3,
    Option.map preproc_else_in_enumerator_list_no_comma_to_poly v4 )

and preproc_if_in_enumerator_list_no_comma_to_poly
    ((v1, v2, v3, v4, v5, v6) : CST.preproc_if_in_enumerator_list_no_comma) :
    CST.enumerator preproc_if_poly =
  ( v1,
    v2,
    v3,
    v4,
    Option.map preproc_else_in_enumerator_list_no_comma_to_poly v5,
    v6 )

and preproc_ifdef_in_enumerator_list_no_comma_to_poly
    ((v1, v2, v3, v4, v5) : CST.preproc_ifdef_in_enumerator_list_no_comma) :
    CST.enumerator preproc_ifdef_poly =
  ( ifdef_token_to_poly v1,
    v2,
    v3,
    Option.map choice_in_enumerator_list_no_comma_to_poly v4,
    v5 )

and preproc_else_in_enumerator_list_no_comma_to_poly
    (x : CST.anon_choice_prep_else_in_enum_list_no_comma_04fd5a5) :
    CST.enumerator preproc_else_poly =
  match x with
  | `Prep_else_in_enum_list_no_comma (v1, v2) -> `Prep_else_poly (v1, v2)
  | `Prep_elif_in_enum_list_no_comma (v1, v2, v3, v4, v5) ->
      `Prep_elif_poly
        ( v1,
          v2,
          v3,
          v4,
          Option.map preproc_else_in_enumerator_list_no_comma_to_poly v5 )
