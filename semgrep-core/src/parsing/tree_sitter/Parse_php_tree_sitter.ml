(* Yoann Padioleau
 *
 * Copyright (C) 2021 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
(*
open Common
module PI = Parse_info
open Cst_cpp
open Ast_c
module G = AST_generic
 *)
module CST = Tree_sitter_php.CST
module H = Parse_tree_sitter_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* PHP parser using tree-sitter-lang/semgrep-php and converting
 * to pfff/lang_php/parsing/ast_php.ml
 *
 * The resulting AST can then be converted to the generic AST by using
 * php_to_generic.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type env = unit H.env

let token = H.token
let _str = H.str

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)

(* This was started by copying tree-sitter-lang/semgrep-php/.../Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the php CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

let todo (env : env) _ = failwith "not implemented"

let map_primitive_type (env : env) (x : CST.primitive_type) =
  match x with
  | `Array tok -> (* "array" *) token env tok
  | `Call tok -> (* "callable" *) token env tok
  | `Iter tok -> (* "iterable" *) token env tok
  | `Bool tok -> (* "bool" *) token env tok
  | `Float tok -> (* "float" *) token env tok
  | `Int tok -> (* "int" *) token env tok
  | `Str tok -> (* "string" *) token env tok
  | `Void tok -> (* "void" *) token env tok
  | `Mixed tok -> (* "mixed" *) token env tok
  | `Static tok -> (* "static" *) token env tok
  | `False tok -> (* "false" *) token env tok
  | `Null tok -> (* "null" *) token env tok

let map_cast_type (env : env) (x : CST.cast_type) =
  match x with
  | `Array tok -> (* "array" *) token env tok
  | `Bin tok -> (* "binary" *) token env tok
  | `Bool_c506ff1 tok -> (* "bool" *) token env tok
  | `Bool_84e2c64 tok -> (* "boolean" *) token env tok
  | `Double tok -> (* "double" *) token env tok
  | `Int_fa7153f tok -> (* "int" *) token env tok
  | `Int_157db7d tok -> (* "integer" *) token env tok
  | `Float tok -> (* "float" *) token env tok
  | `Obj tok -> (* "object" *) token env tok
  | `Real tok -> (* "real" *) token env tok
  | `Str tok -> (* "string" *) token env tok
  | `Unset tok -> (* "unset" *) token env tok

let map_anon_choice_COLON_5102e09 (env : env)
    (x : CST.anon_choice_COLON_5102e09) =
  match x with
  | `COLON tok -> (* ":" *) token env tok
  | `SEMI tok -> (* ";" *) token env tok

let map_text (env : env) (xs : CST.text) =
  List.map
    (fun x ->
      match x with
      | `LT tok -> (* < *) token env tok
      | `Pat_b91d208 tok -> (* pattern [^\s<][^<]* *) token env tok)
    xs

let map_namespace_name (env : env) ((v1, v2) : CST.namespace_name) =
  let v1 =
    (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v1
  in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "\\" *) token env v1 in
        let v2 =
          (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
          token env v2
        in
        todo env (v1, v2))
      v2
  in
  todo env (v1, v2)

let map_named_label_statement (env : env) ((v1, v2) : CST.named_label_statement)
    =
  let v1 =
    (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v1
  in
  let v2 = (* ":" *) token env v2 in
  todo env (v1, v2)

let map_variable_name (env : env) ((v1, v2) : CST.variable_name) =
  let v1 = (* "$" *) token env v1 in
  let v2 =
    (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v2
  in
  todo env (v1, v2)

let map_namespace_aliasing_clause (env : env)
    ((v1, v2) : CST.namespace_aliasing_clause) =
  let v1 = (* pattern [aA][sS] *) token env v1 in
  let v2 =
    (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v2
  in
  todo env (v1, v2)

let map_visibility_modifier (env : env) (x : CST.visibility_modifier) =
  match x with
  | `Pat_a9304a9 tok -> (* pattern [pP][uU][bB][lL][iI][cC] *) token env tok
  | `Pat_954cb76 tok ->
      (* pattern [pP][rR][oO][tT][eE][cC][tT][eE][dD] *) token env tok
  | `Pat_1206b1e tok -> (* pattern [pP][rR][iI][vV][aA][tT][eE] *) token env tok

let map_string__ (env : env) (x : CST.string__) =
  match x with
  | `Str tok -> (* string *) token env tok
  | `Here tok -> (* heredoc *) token env tok

let map_anon_choice_pat_174c3a5_81b85de (env : env)
    (x : CST.anon_choice_pat_174c3a5_81b85de) =
  match x with
  | `Pat_174c3a5 tok ->
      (* pattern [fF][uU][nN][cC][tT][iI][oO][nN] *) token env tok
  | `Pat_7beed81 tok -> (* pattern [cC][oO][nN][sS][tT] *) token env tok

let map_semicolon (env : env) (x : CST.semicolon) =
  match x with
  | `Auto_semi tok -> (* automatic_semicolon *) token env tok
  | `SEMI tok -> (* ";" *) token env tok

let map_namespace_name_as_prefix (env : env) (x : CST.namespace_name_as_prefix)
    =
  match x with
  | `BSLASH tok -> (* "\\" *) token env tok
  | `Opt_BSLASH_name_name_BSLASH (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some tok -> (* "\\" *) token env tok
        | None -> todo env ()
      in
      let v2 = map_namespace_name env v2 in
      let v3 = (* "\\" *) token env v3 in
      todo env (v1, v2, v3)
  | `Pat_1e9d49b_BSLASH (v1, v2) ->
      let v1 =
        (* pattern [nN][aA][mM][eE][sS][pP][aA][cC][eE] *) token env v1
      in
      let v2 = (* "\\" *) token env v2 in
      todo env (v1, v2)
  | `Pat_1e9d49b_opt_BSLASH_name_name_BSLASH (v1, v2, v3, v4) ->
      let v1 =
        (* pattern [nN][aA][mM][eE][sS][pP][aA][cC][eE] *) token env v1
      in
      let v2 =
        match v2 with
        | Some tok -> (* "\\" *) token env tok
        | None -> todo env ()
      in
      let v3 = map_namespace_name env v3 in
      let v4 = (* "\\" *) token env v4 in
      todo env (v1, v2, v3, v4)

let map_anonymous_function_use_clause (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.anonymous_function_use_clause) =
  let v1 = (* pattern [uU][sS][eE] *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    match v3 with
    | Some tok -> (* "&" *) token env tok
    | None -> todo env ()
  in
  let v4 = map_variable_name env v4 in
  let v5 =
    List.map
      (fun (v1, v2, v3) ->
        let v1 = (* "," *) token env v1 in
        let v2 =
          match v2 with
          | Some tok -> (* "&" *) token env tok
          | None -> todo env ()
        in
        let v3 = map_variable_name env v3 in
        todo env (v1, v2, v3))
      v5
  in
  let v6 =
    match v6 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ()
  in
  let v7 = (* ")" *) token env v7 in
  todo env (v1, v2, v3, v4, v5, v6, v7)

let map_literal (env : env) (x : CST.literal) =
  match x with
  | `Int tok -> (* integer *) token env tok
  | `Float tok ->
      (* pattern \d*(_\d+)*((\.\d*(_\d+)*\
         )?([eE][\+-]?\d+(_\d+)*\
         )|(\.\d\d*(_\d+)*\
         )([eE][\+-]?\d+(_\d+)*\
         )?) *)
      token env tok
  | `Str_ x -> map_string__ env x
  | `Bool tok ->
      (* pattern [Tt][Rr][Uu][Ee]|[Ff][Aa][Ll][Ss][Ee] *) token env tok
  | `Null tok -> (* pattern [nN][uU][lL][lL] *) token env tok

let map_namespace_use_group_clause (env : env)
    ((v1, v2, v3) : CST.namespace_use_group_clause) =
  let v1 =
    match v1 with
    | Some x -> map_anon_choice_pat_174c3a5_81b85de env x
    | None -> todo env ()
  in
  let v2 = map_namespace_name env v2 in
  let v3 =
    match v3 with
    | Some x -> map_namespace_aliasing_clause env x
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

let map_modifier (env : env) (x : CST.modifier) =
  match x with
  | `Var_modi tok -> (* pattern [vV][aA][rR] *) token env tok
  | `Visi_modi x -> map_visibility_modifier env x
  | `Static_modi tok -> (* pattern [sS][tT][aA][tT][iI][cC] *) token env tok
  | `Final_modi tok -> (* pattern [fF][iI][nN][aA][lL] *) token env tok
  | `Abst_modi tok ->
      (* pattern [aA][bB][sS][tT][rR][aA][cC][tT] *) token env tok

let map_relative_scope (env : env) (x : CST.relative_scope) =
  match x with
  | `Self tok -> (* "self" *) token env tok
  | `Parent tok -> (* "parent" *) token env tok
  | `Pat_068a1b3 tok -> (* pattern [sS][tT][aA][tT][iI][cC] *) token env tok

let map_reserved_identifier (env : env) (x : CST.reserved_identifier) =
  match x with
  | `Self tok -> (* "self" *) token env tok
  | `Parent tok -> (* "parent" *) token env tok
  | `Pat_068a1b3 tok -> (* pattern [sS][tT][aA][tT][iI][cC] *) token env tok

let map_qualified_name (env : env) ((v1, v2) : CST.qualified_name) =
  let v1 = map_namespace_name_as_prefix env v1 in
  let v2 =
    (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v2
  in
  todo env (v1, v2)

let map_declare_directive (env : env) ((v1, v2, v3) : CST.declare_directive) =
  let v1 =
    match v1 with
    | `Ticks tok -> (* "ticks" *) token env tok
    | `Enco tok -> (* "encoding" *) token env tok
    | `Strict_types tok -> (* "strict_types" *) token env tok
  in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_literal env v3 in
  todo env (v1, v2, v3)

let map_namespace_use_group (env : env)
    ((v1, v2, v3, v4) : CST.namespace_use_group) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_namespace_use_group_clause env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_namespace_use_group_clause env v2 in
        todo env (v1, v2))
      v3
  in
  let v4 = (* "}" *) token env v4 in
  todo env (v1, v2, v3, v4)

let map_anon_choice_name_9dd129a (env : env) (x : CST.anon_choice_name_9dd129a)
    =
  match x with
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      token env tok
  | `Rese_id x -> map_reserved_identifier env x

let map_named_type (env : env) (x : CST.named_type) =
  match x with
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      token env tok
  | `Qual_name x -> map_qualified_name env x

let map_anon_choice_name_062e4f2 (env : env) (x : CST.anon_choice_name_062e4f2)
    =
  match x with
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      token env tok
  | `Rese_id x -> map_reserved_identifier env x
  | `Qual_name x -> map_qualified_name env x

let map_type_list (env : env) ((v1, v2) : CST.type_list) =
  let v1 = map_named_type env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "|" *) token env v1 in
        let v2 = map_named_type env v2 in
        todo env (v1, v2))
      v2
  in
  todo env (v1, v2)

let map_base_clause (env : env) ((v1, v2, v3) : CST.base_clause) =
  let v1 = (* pattern [eE][xX][tT][eE][nN][dD][sS] *) token env v1 in
  let v2 = map_anon_choice_name_062e4f2 env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_name_062e4f2 env v2 in
        todo env (v1, v2))
      v3
  in
  todo env (v1, v2, v3)

let map_class_interface_clause (env : env)
    ((v1, v2, v3) : CST.class_interface_clause) =
  let v1 =
    (* pattern [iI][mM][pP][lL][eE][mM][eE][nN][tT][sS] *) token env v1
  in
  let v2 = map_anon_choice_name_062e4f2 env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_name_062e4f2 env v2 in
        todo env (v1, v2))
      v3
  in
  todo env (v1, v2, v3)

let map_namespace_use_clause (env : env) ((v1, v2) : CST.namespace_use_clause) =
  let v1 = map_anon_choice_name_062e4f2 env v1 in
  let v2 =
    match v2 with
    | Some x -> map_namespace_aliasing_clause env x
    | None -> todo env ()
  in
  todo env (v1, v2)

let map_types (env : env) (x : CST.types) =
  match x with
  | `Opt_type (v1, v2) ->
      let v1 = (* "?" *) token env v1 in
      let v2 =
        match v2 with
        | `Named_type x -> map_named_type env x
        | `Prim_type x -> map_primitive_type env x
      in
      todo env (v1, v2)
  | `Named_type x -> map_named_type env x
  | `Prim_type x -> map_primitive_type env x

let map_union_type (env : env) ((v1, v2) : CST.union_type) =
  let v1 = map_types env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "|" *) token env v1 in
        let v2 = map_types env v2 in
        todo env (v1, v2))
      v2
  in
  todo env (v1, v2)

let map_type_ (env : env) (x : CST.type_) = map_union_type env x

let map_return_type (env : env) ((v1, v2) : CST.return_type) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

let rec map_anon_array_elem_init_rep_COMMA_array_elem_init_1dad3d4 (env : env)
    ((v1, v2) : CST.anon_array_elem_init_rep_COMMA_array_elem_init_1dad3d4) =
  let v1 = map_array_element_initializer env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_array_element_initializer env v2 in
        todo env (v1, v2))
      v2
  in
  todo env (v1, v2)

and map_anon_choice_array_dest_08f4c18 (env : env)
    (x : CST.anon_choice_array_dest_08f4c18) =
  match x with
  | `Array_dest x -> map_array_destructing env x
  | `Choice_cast_var x -> map_variable env x

and map_anon_choice_case_stmt_f1b35bc (env : env)
    (x : CST.anon_choice_case_stmt_f1b35bc) =
  match x with
  | `Case_stmt (v1, v2, v3, v4) ->
      let v1 = (* pattern [cC][aA][sS][eE] *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = map_anon_choice_COLON_5102e09 env v3 in
      let v4 = List.map (map_statement env) v4 in
      todo env (v1, v2, v3, v4)
  | `Defa_stmt (v1, v2, v3) ->
      let v1 = (* pattern [dD][eE][fF][aA][uU][lL][tT] *) token env v1 in
      let v2 = map_anon_choice_COLON_5102e09 env v2 in
      let v3 = List.map (map_statement env) v3 in
      todo env (v1, v2, v3)

and map_anon_choice_choice_array_dest_abfb170 (env : env)
    (x : CST.anon_choice_choice_array_dest_abfb170) =
  match x with
  | `Choice_array_dest x -> map_anon_choice_array_dest_08f4c18 env x
  | `Exp_EQGT_choice_array_dest (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_anon_choice_array_dest_08f4c18 env v3 in
      todo env (v1, v2, v3)

and map_anon_choice_choice_list_dest_c865322 (env : env)
    (x : CST.anon_choice_choice_list_dest_c865322) =
  match x with
  | `Choice_list_dest x -> map_anon_choice_list_dest_bb41c20 env x
  | `Exp_EQGT_choice_list_dest (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_anon_choice_list_dest_bb41c20 env v3 in
      todo env (v1, v2, v3)

and map_anon_choice_class_cst_access_exp_18f5288 (env : env)
    (x : CST.anon_choice_class_cst_access_exp_18f5288) =
  match x with
  | `Class_cst_access_exp x -> map_class_constant_access_expression env x
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      token env tok

and map_anon_choice_list_dest_bb41c20 (env : env)
    (x : CST.anon_choice_list_dest_bb41c20) =
  match x with
  | `List_dest x -> map_list_destructing env x
  | `Choice_cast_var x -> map_variable env x

and map_anon_choice_match_cond_exp_d891119 (env : env)
    (x : CST.anon_choice_match_cond_exp_d891119) =
  match x with
  | `Match_cond_exp (v1, v2, v3) ->
      let v1 = map_match_condition_list env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Match_defa_exp (v1, v2, v3) ->
      let v1 = (* pattern [dD][eE][fF][aA][uU][lL][tT] *) token env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)

and map_anon_choice_simple_param_5af5eb3 (env : env)
    (x : CST.anon_choice_simple_param_5af5eb3) =
  match x with
  | `Simple_param (v1, v2, v3, v4, v5) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> todo env ()
      in
      let v2 =
        match v2 with
        | Some x -> map_type_ env x
        | None -> todo env ()
      in
      let v3 =
        match v3 with
        | Some tok -> (* "&" *) token env tok
        | None -> todo env ()
      in
      let v4 = map_variable_name env v4 in
      let v5 =
        match v5 with
        | Some x -> map_property_initializer env x
        | None -> todo env ()
      in
      todo env (v1, v2, v3, v4, v5)
  | `Vari_param (v1, v2, v3, v4, v5) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> todo env ()
      in
      let v2 =
        match v2 with
        | Some x -> map_type_ env x
        | None -> todo env ()
      in
      let v3 =
        match v3 with
        | Some tok -> (* "&" *) token env tok
        | None -> todo env ()
      in
      let v4 = (* "..." *) token env v4 in
      let v5 = map_variable_name env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Prop_prom_param (v1, v2, v3, v4) ->
      let v1 = map_visibility_modifier env v1 in
      let v2 =
        match v2 with
        | Some x -> map_type_ env x
        | None -> todo env ()
      in
      let v3 = map_variable_name env v3 in
      let v4 =
        match v4 with
        | Some x -> map_property_initializer env x
        | None -> todo env ()
      in
      todo env (v1, v2, v3, v4)

and map_argument (env : env) ((v1, v2) : CST.argument) =
  let v1 =
    match v1 with
    | Some x -> map_named_label_statement env x
    | None -> todo env ()
  in
  let v2 =
    match v2 with
    | `Vari_unpa x -> map_variadic_unpacking env x
    | `Exp x -> map_expression env x
  in
  todo env (v1, v2)

and map_arguments (env : env) ((v1, v2, v3, v4) : CST.arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_argument env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let v1 = (* "," *) token env v1 in
              let v2 = map_argument env v2 in
              todo env (v1, v2))
            v2
        in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v3 =
    match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ()
  in
  let v4 = (* ")" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_array_creation_expression (env : env)
    (x : CST.array_creation_expression) =
  match x with
  | `Array_LPAR_opt_array_elem_init_rep_COMMA_array_elem_init_opt_COMMA_RPAR
      (v1, v2, v3, v4, v5) ->
      let v1 = (* "array" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with
        | Some x ->
            map_anon_array_elem_init_rep_COMMA_array_elem_init_1dad3d4 env x
        | None -> todo env ()
      in
      let v4 =
        match v4 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ()
      in
      let v5 = (* ")" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `LBRACK_opt_array_elem_init_rep_COMMA_array_elem_init_opt_COMMA_RBRACK
      (v1, v2, v3, v4) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        match v2 with
        | Some x ->
            map_anon_array_elem_init_rep_COMMA_array_elem_init_1dad3d4 env x
        | None -> todo env ()
      in
      let v3 =
        match v3 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ()
      in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)

and map_array_destructing (env : env) ((v1, v2, v3, v4) : CST.array_destructing)
    =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_anon_choice_choice_array_dest_abfb170 env x
    | None -> todo env ()
  in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 =
          match v2 with
          | Some x -> map_anon_choice_choice_array_dest_abfb170 env x
          | None -> todo env ()
        in
        todo env (v1, v2))
      v3
  in
  let v4 = (* "]" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_array_element_initializer (env : env)
    (x : CST.array_element_initializer) =
  match x with
  | `Opt_AMP_exp (v1, v2) ->
      let v1 =
        match v1 with
        | Some tok -> (* "&" *) token env tok
        | None -> todo env ()
      in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Exp_EQGT_opt_AMP_exp (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 =
        match v3 with
        | Some tok -> (* "&" *) token env tok
        | None -> todo env ()
      in
      let v4 = map_expression env v4 in
      todo env (v1, v2, v3, v4)
  | `Vari_unpa x -> map_variadic_unpacking env x

and map_attribute (env : env) ((v1, v2) : CST.attribute) =
  let v1 = map_anon_choice_name_062e4f2 env v1 in
  let v2 =
    match v2 with
    | Some x -> map_arguments env x
    | None -> todo env ()
  in
  todo env (v1, v2)

and map_attribute_list (env : env) (xs : CST.attribute_list) =
  List.map
    (fun (v1, v2, v3, v4) ->
      let v1 = (* "#[" *) token env v1 in
      let v2 = map_attribute env v2 in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_attribute env v2 in
            todo env (v1, v2))
          v3
      in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4))
    xs

and map_binary_expression (env : env) (x : CST.binary_expression) =
  match x with
  | `Un_exp_pat_d58874b_choice_qual_name (v1, v2, v3) ->
      let v1 = map_unary_expression env v1 in
      let v2 =
        (* pattern [iI][nN][sS][tT][aA][nN][cC][eE][oO][fF] *) token env v2
      in
      let v3 = map_class_type_designator env v3 in
      todo env (v1, v2, v3)
  | `Exp_QMARKQMARK_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "??" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_pat_e0610ac_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* pattern and|AND *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_pat_48a4c46_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* pattern or|OR *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_pat_f398476_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* pattern xor|XOR *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<>" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQEQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "===" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BANGEQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "!==" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTEQGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<=>" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_DOT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)

and map_callable_expression (env : env) (x : CST.callable_expression) =
  match x with
  | `Choice_choice_dyna_var_name x -> map_callable_variable env x
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Array_crea_exp x -> map_array_creation_expression env x
  | `Str_ x -> map_string__ env x

and map_callable_variable (env : env) (x : CST.callable_variable) =
  match x with
  | `Choice_dyna_var_name x -> map_variable_name_ env x
  | `Subs_exp x -> map_subscript_expression env x
  | `Member_call_exp (v1, v2, v3, v4) ->
      let v1 = map_dereferencable_expression env v1 in
      let v2 = (* "->" *) token env v2 in
      let v3 = map_member_name env v3 in
      let v4 = map_arguments env v4 in
      todo env (v1, v2, v3, v4)
  | `Null_member_call_exp (v1, v2, v3, v4) ->
      let v1 = map_dereferencable_expression env v1 in
      let v2 = (* "?->" *) token env v2 in
      let v3 = map_member_name env v3 in
      let v4 = map_arguments env v4 in
      todo env (v1, v2, v3, v4)
  | `Scoped_call_exp (v1, v2, v3, v4) ->
      let v1 = map_scope_resolution_qualifier env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_member_name env v3 in
      let v4 = map_arguments env v4 in
      todo env (v1, v2, v3, v4)
  | `Func_call_exp (v1, v2) ->
      let v1 =
        match v1 with
        | `Name tok ->
            (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
            token env tok
        | `Rese_id x -> map_reserved_identifier env x
        | `Qual_name x -> map_qualified_name env x
        | `Choice_choice_choice_dyna_var_name x -> map_callable_expression env x
      in
      let v2 = map_arguments env v2 in
      todo env (v1, v2)

and map_catch_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.catch_clause) =
  let v1 = (* pattern [cC][aA][tT][cC][hH] *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_type_list env v3 in
  let v4 =
    match v4 with
    | Some x -> map_variable_name env x
    | None -> todo env ()
  in
  let v5 = (* ")" *) token env v5 in
  let v6 = map_compound_statement env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and map_class_constant_access_expression (env : env)
    ((v1, v2, v3) : CST.class_constant_access_expression) =
  let v1 = map_scope_resolution_qualifier env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 = map_anon_choice_name_9dd129a env v3 in
  todo env (v1, v2, v3)

and map_class_type_designator (env : env) (x : CST.class_type_designator) =
  match x with
  | `Qual_name x -> map_qualified_name env x
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      token env tok
  | `Rese_id x -> map_reserved_identifier env x
  | `Subs_exp x -> map_subscript_expression env x
  | `Member_access_exp x -> map_member_access_expression env x
  | `Null_member_access_exp x -> map_nullsafe_member_access_expression env x
  | `Scoped_prop_access_exp x -> map_scoped_property_access_expression env x
  | `Choice_dyna_var_name x -> map_variable_name_ env x

and map_clone_expression (env : env) ((v1, v2) : CST.clone_expression) =
  let v1 = (* "clone" *) token env v1 in
  let v2 = map_primary_expression env v2 in
  todo env (v1, v2)

and map_colon_block (env : env) ((v1, v2) : CST.colon_block) =
  let v1 = (* ":" *) token env v1 in
  let v2 = List.map (map_statement env) v2 in
  todo env (v1, v2)

and map_compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) =
  let v1 = (* "{" *) token env v1 in
  let v2 = List.map (map_statement env) v2 in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_const_declaration (env : env) (x : CST.const_declaration) =
  map_const_declaration_ env x

and map_const_declaration_ (env : env)
    ((v1, v2, v3, v4, v5) : CST.const_declaration_) =
  let v1 =
    match v1 with
    | Some x -> map_visibility_modifier env x
    | None -> todo env ()
  in
  let v2 = (* pattern [cC][oO][nN][sS][tT] *) token env v2 in
  let v3 = map_const_element env v3 in
  let v4 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_const_element env v2 in
        todo env (v1, v2))
      v4
  in
  let v5 = map_semicolon env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_const_element (env : env) ((v1, v2, v3) : CST.const_element) =
  let v1 = map_anon_choice_name_9dd129a env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_expression env v3 in
  todo env (v1, v2, v3)

and map_declaration_list (env : env) ((v1, v2, v3) : CST.declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 = List.map (map_member_declaration env) v2 in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_dereferencable_expression (env : env)
    (x : CST.dereferencable_expression) =
  match x with
  | `Choice_cast_var x -> map_variable env x
  | `Class_cst_access_exp x -> map_class_constant_access_expression env x
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Array_crea_exp x -> map_array_creation_expression env x
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      token env tok
  | `Rese_id x -> map_reserved_identifier env x
  | `Qual_name x -> map_qualified_name env x
  | `Str_ x -> map_string__ env x

and map_dynamic_variable_name (env : env) (x : CST.dynamic_variable_name) =
  match x with
  | `DOLLAR_choice_dyna_var_name (v1, v2) ->
      let v1 = (* "$" *) token env v1 in
      let v2 = map_variable_name_ env v2 in
      todo env (v1, v2)
  | `DOLLAR_LCURL_exp_RCURL (v1, v2, v3, v4) ->
      let v1 = (* "$" *) token env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "}" *) token env v4 in
      todo env (v1, v2, v3, v4)

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = (* pattern [eE][lL][sS][eE] *) token env v1 in
  let v2 = map_statement env v2 in
  todo env (v1, v2)

and map_else_clause_2 (env : env) ((v1, v2) : CST.else_clause_2) =
  let v1 = (* pattern [eE][lL][sS][eE] *) token env v1 in
  let v2 = map_colon_block env v2 in
  todo env (v1, v2)

and map_else_if_clause (env : env) ((v1, v2, v3) : CST.else_if_clause) =
  let v1 = (* pattern [eE][lL][sS][eE][iI][fF] *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_statement env v3 in
  todo env (v1, v2, v3)

and map_else_if_clause_2 (env : env) ((v1, v2, v3) : CST.else_if_clause_2) =
  let v1 = (* pattern [eE][lL][sS][eE][iI][fF] *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_colon_block env v3 in
  todo env (v1, v2, v3)

and map_enum_declaration_list (env : env)
    ((v1, v2, v3) : CST.enum_declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 = List.map (map_enum_member_declaration env) v2 in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_enum_member_declaration (env : env) (x : CST.enum_member_declaration) =
  match x with
  | `Enum_case (v1, v2, v3, v4, v5) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> todo env ()
      in
      let v2 = (* "case" *) token env v2 in
      let v3 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
        token env v3
      in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            let v1 = (* "=" *) token env v1 in
            let v2 =
              match v2 with
              | `Str tok -> (* string *) token env tok
              | `Int tok -> (* integer *) token env tok
            in
            todo env (v1, v2)
        | None -> todo env ()
      in
      let v5 = map_semicolon env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Meth_decl x -> map_method_declaration env x
  | `Use_decl x -> map_use_declaration env x

and map_exponentiation_expression (env : env)
    ((v1, v2, v3) : CST.exponentiation_expression) =
  let v1 =
    match v1 with
    | `Clone_exp x -> map_clone_expression env x
    | `Prim_exp x -> map_primary_expression env x
  in
  let v2 = (* "**" *) token env v2 in
  let v3 =
    match v3 with
    | `Expo_exp x -> map_exponentiation_expression env x
    | `Clone_exp x -> map_clone_expression env x
    | `Prim_exp x -> map_primary_expression env x
  in
  todo env (v1, v2, v3)

and map_expression (env : env) (x : CST.expression) =
  match x with
  | `Cond_exp (v1, v2, v3, v4, v5) ->
      let v1 = map_expression env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> map_expression env x
        | None -> todo env ()
      in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Match_exp (v1, v2, v3) ->
      let v1 = (* pattern [mM][aA][tT][cC][hH] *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_match_block env v3 in
      todo env (v1, v2, v3)
  | `Augm_assign_exp (v1, v2, v3) ->
      let v1 = map_variable env v1 in
      let v2 =
        match v2 with
        | `STARSTAREQ tok -> (* "**=" *) token env tok
        | `STAREQ tok -> (* "*=" *) token env tok
        | `SLASHEQ tok -> (* "/=" *) token env tok
        | `PERCEQ tok -> (* "%=" *) token env tok
        | `PLUSEQ tok -> (* "+=" *) token env tok
        | `DASHEQ tok -> (* "-=" *) token env tok
        | `DOTEQ tok -> (* ".=" *) token env tok
        | `LTLTEQ tok -> (* "<<=" *) token env tok
        | `GTGTEQ tok -> (* ">>=" *) token env tok
        | `AMPEQ tok -> (* "&=" *) token env tok
        | `HATEQ tok -> (* "^=" *) token env tok
        | `BAREQ tok -> (* "|=" *) token env tok
        | `QMARKQMARKEQ tok -> (* "??=" *) token env tok
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Assign_exp (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | `Choice_cast_var x -> map_variable env x
        | `List_lit x -> map_list_literal env x
      in
      let v2 = (* "=" *) token env v2 in
      let v3 =
        match v3 with
        | Some tok -> (* "&" *) token env tok
        | None -> todo env ()
      in
      let v4 = map_expression env v4 in
      todo env (v1, v2, v3, v4)
  | `Yield_exp (v1, v2) ->
      let v1 = (* "yield" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> (
            match x with
            | `Array_elem_init x -> map_array_element_initializer env x
            | `From_exp (v1, v2) ->
                let v1 = (* "from" *) token env v1 in
                let v2 = map_expression env v2 in
                todo env (v1, v2))
        | None -> todo env ()
      in
      todo env (v1, v2)
  | `Un_exp x -> map_unary_expression env x
  | `Bin_exp x -> map_binary_expression env x
  | `Incl_exp (v1, v2) ->
      let v1 = (* pattern [iI][nN][cC][lL][uU][dD][eE] *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Incl_once_exp (v1, v2) ->
      let v1 =
        (* pattern [iI][nN][cC][lL][uU][dD][eE][__][oO][nN][cC][eE] *)
        token env v1
      in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Requ_exp (v1, v2) ->
      let v1 = (* pattern [rR][eE][qQ][uU][iI][rR][eE] *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Requ_once_exp (v1, v2) ->
      let v1 =
        (* pattern [rR][eE][qQ][uU][iI][rR][eE][__][oO][nN][cC][eE] *)
        token env v1
      in
      let v2 = map_expression env v2 in
      todo env (v1, v2)

and map_expressions (env : env) (x : CST.expressions) =
  match x with
  | `Exp x -> map_expression env x
  | `Seq_exp x -> map_sequence_expression env x

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = (* pattern [fF][iI][nN][aA][lL][lL][yY] *) token env v1 in
  let v2 = map_compound_statement env v2 in
  todo env (v1, v2)

and map_foreach_pair (env : env) ((v1, v2, v3) : CST.foreach_pair) =
  let v1 = map_expression env v1 in
  let v2 = (* "=>" *) token env v2 in
  let v3 = map_foreach_value env v3 in
  todo env (v1, v2, v3)

and map_foreach_value (env : env) (x : CST.foreach_value) =
  match x with
  | `Opt_AMP_exp (v1, v2) ->
      let v1 =
        match v1 with
        | Some tok -> (* "&" *) token env tok
        | None -> todo env ()
      in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `List_lit x -> map_list_literal env x

and map_formal_parameters (env : env) ((v1, v2, v3, v4) : CST.formal_parameters)
    =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_simple_param_5af5eb3 env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let v1 = (* "," *) token env v1 in
              let v2 = map_anon_choice_simple_param_5af5eb3 env v2 in
              todo env (v1, v2))
            v2
        in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v3 =
    match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ()
  in
  let v4 = (* ")" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_function_definition_header (env : env)
    ((v1, v2, v3, v4, v5) : CST.function_definition_header) =
  let v1 = (* pattern [fF][uU][nN][cC][tT][iI][oO][nN] *) token env v1 in
  let v2 =
    match v2 with
    | Some tok -> (* "&" *) token env tok
    | None -> todo env ()
  in
  let v3 = map_anon_choice_name_9dd129a env v3 in
  let v4 = map_formal_parameters env v4 in
  let v5 =
    match v5 with
    | Some x -> map_return_type env x
    | None -> todo env ()
  in
  todo env (v1, v2, v3, v4, v5)

and map_list_destructing (env : env)
    ((v1, v2, v3, v4, v5) : CST.list_destructing) =
  let v1 = (* "list" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    match v3 with
    | Some x -> map_anon_choice_choice_list_dest_c865322 env x
    | None -> todo env ()
  in
  let v4 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 =
          match v2 with
          | Some x -> map_anon_choice_choice_list_dest_c865322 env x
          | None -> todo env ()
        in
        todo env (v1, v2))
      v4
  in
  let v5 = (* ")" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_list_literal (env : env) (x : CST.list_literal) =
  match x with
  | `List_dest x -> map_list_destructing env x
  | `Array_dest x -> map_array_destructing env x

and map_match_block (env : env) ((v1, v2, v3, v4, v5) : CST.match_block) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_anon_choice_match_cond_exp_d891119 env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_match_cond_exp_d891119 env v2 in
        todo env (v1, v2))
      v3
  in
  let v4 =
    match v4 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ()
  in
  let v5 = (* "}" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_match_condition_list (env : env) ((v1, v2) : CST.match_condition_list) =
  let v1 = map_expression env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_expression env v2 in
        todo env (v1, v2))
      v2
  in
  todo env (v1, v2)

and map_member_access_expression (env : env)
    ((v1, v2, v3) : CST.member_access_expression) =
  let v1 = map_dereferencable_expression env v1 in
  let v2 = (* "->" *) token env v2 in
  let v3 = map_member_name env v3 in
  todo env (v1, v2, v3)

and map_member_declaration (env : env) (x : CST.member_declaration) =
  match x with
  | `Class_const_decl (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> todo env ()
      in
      let v2 =
        match v2 with
        | Some tok -> (* pattern [fF][iI][nN][aA][lL] *) token env tok
        | None -> todo env ()
      in
      let v3 = map_const_declaration env v3 in
      todo env (v1, v2, v3)
  | `Prop_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> todo env ()
      in
      let v2 = List.map (map_modifier env) v2 in
      let v3 =
        match v3 with
        | Some x -> map_type_ env x
        | None -> todo env ()
      in
      let v4 = map_property_element env v4 in
      let v5 =
        List.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_property_element env v2 in
            todo env (v1, v2))
          v5
      in
      let v6 = map_semicolon env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Meth_decl x -> map_method_declaration env x
  | `Use_decl x -> map_use_declaration env x

and map_member_name (env : env) (x : CST.member_name) =
  match x with
  | `Choice_rese_id x -> (
      match x with
      | `Rese_id x -> map_reserved_identifier env x
      | `Name tok ->
          (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
          token env tok
      | `Choice_dyna_var_name x -> map_variable_name_ env x)
  | `LCURL_exp_RCURL (v1, v2, v3) ->
      let v1 = (* "{" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "}" *) token env v3 in
      todo env (v1, v2, v3)

and map_method_declaration (env : env)
    ((v1, v2, v3, v4) : CST.method_declaration) =
  let v1 =
    match v1 with
    | Some x -> map_attribute_list env x
    | None -> todo env ()
  in
  let v2 = List.map (map_modifier env) v2 in
  let v3 = map_function_definition_header env v3 in
  let v4 =
    match v4 with
    | `Comp_stmt x -> map_compound_statement env x
    | `Choice_auto_semi x -> map_semicolon env x
  in
  todo env (v1, v2, v3, v4)

and map_nullsafe_member_access_expression (env : env)
    ((v1, v2, v3) : CST.nullsafe_member_access_expression) =
  let v1 = map_dereferencable_expression env v1 in
  let v2 = (* "?->" *) token env v2 in
  let v3 = map_member_name env v3 in
  todo env (v1, v2, v3)

and map_object_creation_expression (env : env)
    (x : CST.object_creation_expression) =
  match x with
  | `New_choice_qual_name_opt_args (v1, v2, v3) ->
      let v1 = (* "new" *) token env v1 in
      let v2 = map_class_type_designator env v2 in
      let v3 =
        match v3 with
        | Some x -> map_arguments env x
        | None -> todo env ()
      in
      todo env (v1, v2, v3)
  | `New_pat_a7a1629_opt_args_opt_base_clause_opt_class_inte_clause_decl_list
      (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "new" *) token env v1 in
      let v2 = (* pattern [cC][lL][aA][sS][sS] *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> map_arguments env x
        | None -> todo env ()
      in
      let v4 =
        match v4 with
        | Some x -> map_base_clause env x
        | None -> todo env ()
      in
      let v5 =
        match v5 with
        | Some x -> map_class_interface_clause env x
        | None -> todo env ()
      in
      let v6 = map_declaration_list env v6 in
      todo env (v1, v2, v3, v4, v5, v6)

and map_parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_primary_expression (env : env) (x : CST.primary_expression) =
  match x with
  | `Choice_cast_var x -> map_variable env x
  | `Choice_int x -> map_literal env x
  | `Class_cst_access_exp x -> map_class_constant_access_expression env x
  | `Qual_name x -> map_qualified_name env x
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      token env tok
  | `Array_crea_exp x -> map_array_creation_expression env x
  | `Print_intr (v1, v2) ->
      let v1 = (* "print" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Anon_func_crea_exp (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        match v1 with
        | Some tok -> (* pattern [sS][tT][aA][tT][iI][cC] *) token env tok
        | None -> todo env ()
      in
      let v2 = (* pattern [fF][uU][nN][cC][tT][iI][oO][nN] *) token env v2 in
      let v3 =
        match v3 with
        | Some tok -> (* "&" *) token env tok
        | None -> todo env ()
      in
      let v4 = map_formal_parameters env v4 in
      let v5 =
        match v5 with
        | Some x -> map_anonymous_function_use_clause env x
        | None -> todo env ()
      in
      let v6 =
        match v6 with
        | Some x -> map_return_type env x
        | None -> todo env ()
      in
      let v7 = map_compound_statement env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Arrow_func (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        match v1 with
        | Some tok -> (* pattern [sS][tT][aA][tT][iI][cC] *) token env tok
        | None -> todo env ()
      in
      let v2 = (* pattern [fF][nN] *) token env v2 in
      let v3 =
        match v3 with
        | Some tok -> (* "&" *) token env tok
        | None -> todo env ()
      in
      let v4 = map_formal_parameters env v4 in
      let v5 =
        match v5 with
        | Some x -> map_return_type env x
        | None -> todo env ()
      in
      let v6 = (* "=>" *) token env v6 in
      let v7 = map_expression env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Obj_crea_exp x -> map_object_creation_expression env x
  | `Update_exp x -> map_update_expression env x
  | `Shell_cmd_exp tok -> (* shell_command_expression *) token env tok
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Throw_exp (v1, v2) ->
      let v1 = (* pattern [tT][hH][rR][oO][wW] *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)

and map_property_element (env : env) ((v1, v2) : CST.property_element) =
  let v1 = map_variable_name env v1 in
  let v2 =
    match v2 with
    | Some x -> map_property_initializer env x
    | None -> todo env ()
  in
  todo env (v1, v2)

and map_property_initializer (env : env) ((v1, v2) : CST.property_initializer) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_scope_resolution_qualifier (env : env)
    (x : CST.scope_resolution_qualifier) =
  match x with
  | `Rela_scope x -> map_relative_scope env x
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      token env tok
  | `Rese_id x -> map_reserved_identifier env x
  | `Qual_name x -> map_qualified_name env x
  | `Dere_exp x -> map_dereferencable_expression env x

and map_scoped_property_access_expression (env : env)
    ((v1, v2, v3) : CST.scoped_property_access_expression) =
  let v1 = map_scope_resolution_qualifier env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 = map_variable_name_ env v3 in
  todo env (v1, v2, v3)

and map_sequence_expression (env : env) ((v1, v2, v3) : CST.sequence_expression)
    =
  let v1 = map_expression env v1 in
  let v2 = (* "," *) token env v2 in
  let v3 =
    match v3 with
    | `Seq_exp x -> map_sequence_expression env x
    | `Exp x -> map_expression env x
  in
  todo env (v1, v2, v3)

and map_statement (env : env) (x : CST.statement) =
  match x with
  | `Empty_stmt tok -> (* ";" *) token env tok
  | `Comp_stmt x -> map_compound_statement env x
  | `Named_label_stmt x -> map_named_label_statement env x
  | `Exp_stmt (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_semicolon env v2 in
      todo env (v1, v2)
  | `If_stmt (v1, v2, v3) ->
      let v1 = (* pattern [iI][fF] *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 =
        match v3 with
        | `Choice_empty_stmt_rep_else_if_clause_opt_else_clause (v1, v2, v3) ->
            let v1 = map_statement env v1 in
            let v2 = List.map (map_else_if_clause env) v2 in
            let v3 =
              match v3 with
              | Some x -> map_else_clause env x
              | None -> todo env ()
            in
            todo env (v1, v2, v3)
        | `Colon_blk_rep_else_if_clause_2_opt_else_clause_2_pat_b10beb6_choice_auto_semi
            (v1, v2, v3, v4, v5) ->
            let v1 = map_colon_block env v1 in
            let v2 = List.map (map_else_if_clause_2 env) v2 in
            let v3 =
              match v3 with
              | Some x -> map_else_clause_2 env x
              | None -> todo env ()
            in
            let v4 = (* pattern [eE][nN][dD][iI][fF] *) token env v4 in
            let v5 = map_semicolon env v5 in
            todo env (v1, v2, v3, v4, v5)
      in
      todo env (v1, v2, v3)
  | `Switch_stmt (v1, v2, v3) ->
      let v1 = (* pattern [sS][wW][iI][tT][cC][hH] *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_switch_block env v3 in
      todo env (v1, v2, v3)
  | `While_stmt (v1, v2, v3) ->
      let v1 = (* pattern [wW][hH][iI][lL][eE] *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 =
        match v3 with
        | `Choice_empty_stmt x -> map_statement env x
        | `Colon_blk_pat_2bdb730_choice_auto_semi (v1, v2, v3) ->
            let v1 = map_colon_block env v1 in
            let v2 =
              (* pattern [eE][nN][dD][wW][hH][iI][lL][eE] *) token env v2
            in
            let v3 = map_semicolon env v3 in
            todo env (v1, v2, v3)
      in
      todo env (v1, v2, v3)
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* pattern [dD][oO] *) token env v1 in
      let v2 = map_statement env v2 in
      let v3 = (* pattern [wW][hH][iI][lL][eE] *) token env v3 in
      let v4 = map_parenthesized_expression env v4 in
      let v5 = map_semicolon env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = (* pattern [fF][oO][rR] *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> map_expressions env x
        | None -> todo env ()
      in
      let v4 = (* ";" *) token env v4 in
      let v5 =
        match v5 with
        | Some x -> map_expressions env x
        | None -> todo env ()
      in
      let v6 = (* ";" *) token env v6 in
      let v7 =
        match v7 with
        | Some x -> map_expressions env x
        | None -> todo env ()
      in
      let v8 = (* ")" *) token env v8 in
      let v9 =
        match v9 with
        | `Choice_auto_semi x -> map_semicolon env x
        | `Choice_empty_stmt x -> map_statement env x
        | `COLON_rep_choice_empty_stmt_pat_1d5f5b3_choice_auto_semi
            (v1, v2, v3, v4) ->
            let v1 = (* ":" *) token env v1 in
            let v2 = List.map (map_statement env) v2 in
            let v3 = (* pattern [eE][nN][dD][fF][oO][rR] *) token env v3 in
            let v4 = map_semicolon env v4 in
            todo env (v1, v2, v3, v4)
      in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  | `Fore_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = (* pattern [fF][oO][rR][eE][aA][cC][hH] *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* pattern [aA][sS] *) token env v4 in
      let v5 =
        match v5 with
        | `Fore_pair x -> map_foreach_pair env x
        | `Choice_opt_AMP_exp x -> map_foreach_value env x
      in
      let v6 = (* ")" *) token env v6 in
      let v7 =
        match v7 with
        | `Choice_auto_semi x -> map_semicolon env x
        | `Choice_empty_stmt x -> map_statement env x
        | `Colon_blk_pat_25e0188_choice_auto_semi (v1, v2, v3) ->
            let v1 = map_colon_block env v1 in
            let v2 =
              (* pattern [eE][nN][dD][fF][oO][rR][eE][aA][cC][hH] *)
              token env v2
            in
            let v3 = map_semicolon env v3 in
            todo env (v1, v2, v3)
      in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Goto_stmt (v1, v2, v3) ->
      let v1 = (* pattern [gG][oO][tT][oO] *) token env v1 in
      let v2 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
        token env v2
      in
      let v3 = map_semicolon env v3 in
      todo env (v1, v2, v3)
  | `Cont_stmt (v1, v2, v3) ->
      let v1 = (* pattern [cC][oO][nN][tT][iI][nN][uU][eE] *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_expression env x
        | None -> todo env ()
      in
      let v3 = map_semicolon env v3 in
      todo env (v1, v2, v3)
  | `Brk_stmt (v1, v2, v3) ->
      let v1 = (* pattern [bB][rR][eE][aA][kK] *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_expression env x
        | None -> todo env ()
      in
      let v3 = map_semicolon env v3 in
      todo env (v1, v2, v3)
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = (* pattern [rR][eE][tT][uU][rR][nN] *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_expression env x
        | None -> todo env ()
      in
      let v3 = map_semicolon env v3 in
      todo env (v1, v2, v3)
  | `Try_stmt (v1, v2, v3) ->
      let v1 = (* pattern [tT][rR][yY] *) token env v1 in
      let v2 = map_compound_statement env v2 in
      let v3 =
        List.map
          (fun x ->
            match x with
            | `Catch_clause x -> map_catch_clause env x
            | `Fina_clause x -> map_finally_clause env x)
          v3
      in
      todo env (v1, v2, v3)
  | `Decl_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "declare" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_declare_directive env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 =
        match v5 with
        | `Choice_empty_stmt x -> map_statement env x
        | `COLON_rep_choice_empty_stmt_pat_bb9603f_choice_auto_semi
            (v1, v2, v3, v4) ->
            let v1 = (* ":" *) token env v1 in
            let v2 = List.map (map_statement env) v2 in
            let v3 =
              (* pattern [eE][nN][dD][dD][eE][cC][lL][aA][rR][eE] *)
              token env v3
            in
            let v4 = map_semicolon env v4 in
            todo env (v1, v2, v3, v4)
        | `Choice_auto_semi x -> map_semicolon env x
      in
      todo env (v1, v2, v3, v4, v5)
  | `Echo_stmt (v1, v2, v3) ->
      let v1 = (* pattern [eE][cC][hH][oO] *) token env v1 in
      let v2 = map_expressions env v2 in
      let v3 = map_semicolon env v3 in
      todo env (v1, v2, v3)
  | `Unset_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "unset" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_variable env v3 in
      let v4 =
        List.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_variable env v2 in
            todo env (v1, v2))
          v4
      in
      let v5 = (* ")" *) token env v5 in
      let v6 = map_semicolon env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Const_decl x -> map_const_declaration env x
  | `Func_defi (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> todo env ()
      in
      let v2 = map_function_definition_header env v2 in
      let v3 = map_compound_statement env v3 in
      todo env (v1, v2, v3)
  | `Class_decl (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> todo env ()
      in
      let v2 =
        match v2 with
        | Some x -> (
            match x with
            | `Final_modi tok ->
                (* pattern [fF][iI][nN][aA][lL] *) token env tok
            | `Abst_modi tok ->
                (* pattern [aA][bB][sS][tT][rR][aA][cC][tT] *) token env tok)
        | None -> todo env ()
      in
      let v3 = (* pattern [cC][lL][aA][sS][sS] *) token env v3 in
      let v4 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
        token env v4
      in
      let v5 =
        match v5 with
        | Some x -> map_base_clause env x
        | None -> todo env ()
      in
      let v6 =
        match v6 with
        | Some x -> map_class_interface_clause env x
        | None -> todo env ()
      in
      let v7 = map_declaration_list env v7 in
      let v8 =
        match v8 with
        | Some x -> map_semicolon env x
        | None -> todo env ()
      in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `Inte_decl (v1, v2, v3, v4) ->
      let v1 =
        (* pattern [iI][nN][tT][eE][rR][fF][aA][cC][eE] *) token env v1
      in
      let v2 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
        token env v2
      in
      let v3 =
        match v3 with
        | Some x -> map_base_clause env x
        | None -> todo env ()
      in
      let v4 = map_declaration_list env v4 in
      todo env (v1, v2, v3, v4)
  | `Trait_decl (v1, v2, v3) ->
      let v1 = (* pattern [tT][rR][aA][iI][tT] *) token env v1 in
      let v2 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
        token env v2
      in
      let v3 = map_declaration_list env v3 in
      todo env (v1, v2, v3)
  | `Enum_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> todo env ()
      in
      let v2 = (* pattern [eE][nN][uU][mM] *) token env v2 in
      let v3 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
        token env v3
      in
      let v4 =
        match v4 with
        | Some x -> map_return_type env x
        | None -> todo env ()
      in
      let v5 =
        match v5 with
        | Some x -> map_class_interface_clause env x
        | None -> todo env ()
      in
      let v6 = map_enum_declaration_list env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Name_defi (v1, v2) ->
      let v1 =
        (* pattern [nN][aA][mM][eE][sS][pP][aA][cC][eE] *) token env v1
      in
      let v2 =
        match v2 with
        | `Name_name_choice_auto_semi (v1, v2) ->
            let v1 = map_namespace_name env v1 in
            let v2 = map_semicolon env v2 in
            todo env (v1, v2)
        | `Opt_name_name_comp_stmt (v1, v2) ->
            let v1 =
              match v1 with
              | Some x -> map_namespace_name env x
              | None -> todo env ()
            in
            let v2 = map_compound_statement env v2 in
            todo env (v1, v2)
      in
      todo env (v1, v2)
  | `Name_use_decl (v1, v2, v3, v4) ->
      let v1 = (* pattern [uU][sS][eE] *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_anon_choice_pat_174c3a5_81b85de env x
        | None -> todo env ()
      in
      let v3 =
        match v3 with
        | `Name_use_clause_rep_COMMA_name_use_clause (v1, v2) ->
            let v1 = map_namespace_use_clause env v1 in
            let v2 =
              List.map
                (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = map_namespace_use_clause env v2 in
                  todo env (v1, v2))
                v2
            in
            todo env (v1, v2)
        | `Opt_BSLASH_name_name_BSLASH_name_use_group (v1, v2, v3, v4) ->
            let v1 =
              match v1 with
              | Some tok -> (* "\\" *) token env tok
              | None -> todo env ()
            in
            let v2 = map_namespace_name env v2 in
            let v3 = (* "\\" *) token env v3 in
            let v4 = map_namespace_use_group env v4 in
            todo env (v1, v2, v3, v4)
      in
      let v4 = map_semicolon env v4 in
      todo env (v1, v2, v3, v4)
  | `Global_decl (v1, v2, v3, v4) ->
      let v1 = (* pattern [gG][lL][oO][bB][aA][lL] *) token env v1 in
      let v2 = map_variable_name_ env v2 in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_variable_name_ env v2 in
            todo env (v1, v2))
          v3
      in
      let v4 = map_semicolon env v4 in
      todo env (v1, v2, v3, v4)
  | `Func_static_decl (v1, v2, v3, v4) ->
      let v1 = (* pattern [sS][tT][aA][tT][iI][cC] *) token env v1 in
      let v2 = map_static_variable_declaration env v2 in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_static_variable_declaration env v2 in
            todo env (v1, v2))
          v3
      in
      let v4 = map_semicolon env v4 in
      todo env (v1, v2, v3, v4)

and map_static_variable_declaration (env : env)
    ((v1, v2) : CST.static_variable_declaration) =
  let v1 = map_variable_name env v1 in
  let v2 =
    match v2 with
    | Some x -> map_property_initializer env x
    | None -> todo env ()
  in
  todo env (v1, v2)

and map_subscript_expression (env : env) ((v1, v2) : CST.subscript_expression) =
  let v1 = map_dereferencable_expression env v1 in
  let v2 =
    match v2 with
    | `LBRACK_opt_exp_RBRACK (v1, v2, v3) ->
        let v1 = (* "[" *) token env v1 in
        let v2 =
          match v2 with
          | Some x -> map_expression env x
          | None -> todo env ()
        in
        let v3 = (* "]" *) token env v3 in
        todo env (v1, v2, v3)
    | `LCURL_exp_RCURL (v1, v2, v3) ->
        let v1 = (* "{" *) token env v1 in
        let v2 = map_expression env v2 in
        let v3 = (* "}" *) token env v3 in
        todo env (v1, v2, v3)
  in
  todo env (v1, v2)

and map_switch_block (env : env) (x : CST.switch_block) =
  match x with
  | `LCURL_rep_choice_case_stmt_RCURL (v1, v2, v3) ->
      let v1 = (* "{" *) token env v1 in
      let v2 = List.map (map_anon_choice_case_stmt_f1b35bc env) v2 in
      let v3 = (* "}" *) token env v3 in
      todo env (v1, v2, v3)
  | `COLON_rep_choice_case_stmt_pat_0b47e00_choice_auto_semi (v1, v2, v3, v4) ->
      let v1 = (* ":" *) token env v1 in
      let v2 = List.map (map_anon_choice_case_stmt_f1b35bc env) v2 in
      let v3 =
        (* pattern [eE][nN][dD][sS][wW][iI][tT][cC][hH] *) token env v3
      in
      let v4 = map_semicolon env v4 in
      todo env (v1, v2, v3, v4)

and map_unary_expression (env : env) (x : CST.unary_expression) =
  match x with
  | `Clone_exp x -> map_clone_expression env x
  | `Prim_exp x -> map_primary_expression env x
  | `Expo_exp x -> map_exponentiation_expression env x
  | `Un_op_exp x -> map_unary_op_expression env x
  | `Cast_exp (v1, v2, v3, v4) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_cast_type env v2 in
      let v3 = (* ")" *) token env v3 in
      let v4 = map_unary_expression env v4 in
      todo env (v1, v2, v3, v4)

and map_unary_op_expression (env : env) (x : CST.unary_op_expression) =
  match x with
  | `AT_exp (v1, v2) ->
      let v1 = (* "@" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Choice_PLUS_exp (v1, v2) ->
      let v1 =
        match v1 with
        | `PLUS tok -> (* "+" *) token env tok
        | `DASH tok -> (* "-" *) token env tok
        | `TILDE tok -> (* "~" *) token env tok
        | `BANG tok -> (* "!" *) token env tok
      in
      let v2 = map_expression env v2 in
      todo env (v1, v2)

and map_update_expression (env : env) (x : CST.update_expression) =
  match x with
  | `Choice_cast_var_PLUSPLUS (v1, v2) ->
      let v1 = map_variable env v1 in
      let v2 = (* "++" *) token env v2 in
      todo env (v1, v2)
  | `Choice_cast_var_DASHDASH (v1, v2) ->
      let v1 = map_variable env v1 in
      let v2 = (* "--" *) token env v2 in
      todo env (v1, v2)
  | `PLUSPLUS_choice_cast_var (v1, v2) ->
      let v1 = (* "++" *) token env v1 in
      let v2 = map_variable env v2 in
      todo env (v1, v2)
  | `DASHDASH_choice_cast_var (v1, v2) ->
      let v1 = (* "--" *) token env v1 in
      let v2 = map_variable env v2 in
      todo env (v1, v2)

and map_use_as_clause (env : env) ((v1, v2, v3) : CST.use_as_clause) =
  let v1 = map_anon_choice_class_cst_access_exp_18f5288 env v1 in
  let v2 = (* pattern [aA][sS] *) token env v2 in
  let v3 =
    match v3 with
    | `Opt_visi_modi_name (v1, v2) ->
        let v1 =
          match v1 with
          | Some x -> map_visibility_modifier env x
          | None -> todo env ()
        in
        let v2 =
          (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
          token env v2
        in
        todo env (v1, v2)
    | `Visi_modi_opt_name (v1, v2) ->
        let v1 = map_visibility_modifier env v1 in
        let v2 =
          match v2 with
          | Some tok ->
              (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
              token env tok
          | None -> todo env ()
        in
        todo env (v1, v2)
  in
  todo env (v1, v2, v3)

and map_use_declaration (env : env) ((v1, v2, v3, v4) : CST.use_declaration) =
  let v1 = (* pattern [uU][sS][eE] *) token env v1 in
  let v2 = map_anon_choice_name_062e4f2 env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_name_062e4f2 env v2 in
        todo env (v1, v2))
      v3
  in
  let v4 =
    match v4 with
    | `Use_list x -> map_use_list env x
    | `Choice_auto_semi x -> map_semicolon env x
  in
  todo env (v1, v2, v3, v4)

and map_use_instead_of_clause (env : env)
    ((v1, v2, v3) : CST.use_instead_of_clause) =
  let v1 = map_anon_choice_class_cst_access_exp_18f5288 env v1 in
  let v2 = (* pattern [iI][nN][sS][tT][eE][aA][dD][oO][fF] *) token env v2 in
  let v3 =
    (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v3
  in
  todo env (v1, v2, v3)

and map_use_list (env : env) ((v1, v2, v3) : CST.use_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 =
          match v1 with
          | `Use_inst_of_clause x -> map_use_instead_of_clause env x
          | `Use_as_clause x -> map_use_as_clause env x
        in
        let v2 = map_semicolon env v2 in
        todo env (v1, v2))
      v2
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_variable (env : env) (x : CST.variable) =
  match x with
  | `Cast_var (v1, v2, v3, v4) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_cast_type env v2 in
      let v3 = (* ")" *) token env v3 in
      let v4 = map_variable env v4 in
      todo env (v1, v2, v3, v4)
  | `Choice_choice_dyna_var_name x -> map_callable_variable env x
  | `Scoped_prop_access_exp x -> map_scoped_property_access_expression env x
  | `Member_access_exp x -> map_member_access_expression env x
  | `Null_member_access_exp x -> map_nullsafe_member_access_expression env x

and map_variable_name_ (env : env) (x : CST.variable_name_) =
  match x with
  | `Dyna_var_name x -> map_dynamic_variable_name env x
  | `Var_name x -> map_variable_name env x

and map_variadic_unpacking (env : env) ((v1, v2) : CST.variadic_unpacking) =
  let v1 = (* "..." *) token env v1 in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

let map_program (env : env) ((v1, v2) : CST.program) =
  let v1 =
    match v1 with
    | Some x -> map_text env x
    | None -> todo env ()
  in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = (* pattern <\?([pP][hH][pP]|=)? *) token env v1 in
        let v2 = List.map (map_statement env) v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  todo env (v1, v2)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_php.Parse.file file)
    (fun cst ->
      let extra = () in
      let env = { H.file; conv = H.line_col_to_pos file; extra } in
      map_program env cst)
