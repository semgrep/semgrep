(*
   Copyright (c) 2020-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Set of functions to match a Pattern against some code.
 *
 * Both are represented by an AST_generic type but the pattern
 * can contain construct such as Ellipsis that are not possible
 * to have in the code.
 *
 * See Matching_generic.matcher for more information.
 *)

(* entry points, used in the sgrep_generic visitors *)
val m_expr_root : AST_generic.expr Matching_generic.matcher
val m_stmt : AST_generic.stmt Matching_generic.matcher

val m_stmts_deep :
  inside:bool ->
  less_is_ok:bool ->
  AST_generic.stmt list Matching_generic.matcher

val m_type_ : AST_generic.type_ Matching_generic.matcher
val m_pattern : AST_generic.pattern Matching_generic.matcher
val m_attribute : AST_generic.attribute Matching_generic.matcher
val m_xml_attr : AST_generic.xml_attribute Matching_generic.matcher
val m_partial : AST_generic.partial Matching_generic.matcher
val m_field : AST_generic.field Matching_generic.matcher
val m_fields : AST_generic.field list Matching_generic.matcher
val m_name : AST_generic.name Matching_generic.matcher
val m_raw_tree : AST_generic.raw_tree Matching_generic.matcher

(* used only for unit testing *)
val m_any : AST_generic.any Matching_generic.matcher

(* deep-scan hook *)
val hook_find_possible_parents :
  (AST_generic.dotted_ident -> AST_generic.name list) option Hook.t

(* deep-scan hook *)
val hook_match_PatConstructor :
  (default:(unit -> Matching_generic.tin -> Matching_generic.tout) ->
  m_pattern:
    (AST_generic.pattern, AST_generic.pattern) Matching_generic.general_matcher ->
  AST_generic.name * AST_generic.pattern list ->
  AST_generic.name * AST_generic.pattern list ->
  Matching_generic.tin ->
  Matching_generic.tout)
  option
  Hook.t

val hook_r2c_pro_was_here : bool Hook.t

(* used for evaluating `metavariable-type:` in Match_search_mode.ml *)
val m_compatible_type :
  Lang.t ->
  AST_generic.ident ->
  AST_generic.type_ ->
  AST_generic.expr ->
  Matching_generic.tin ->
  Matching_generic.tout
