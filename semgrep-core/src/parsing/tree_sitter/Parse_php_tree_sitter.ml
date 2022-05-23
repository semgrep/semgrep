(* Yoann Padioleau
 *
 * Copyright (C) 2021 r2c
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
(*
open Common
module PI = Parse_info
open Cst_cpp
open Ast_c
*)
module CST = Tree_sitter_php.CST
module H = Parse_tree_sitter_helpers
module A = Ast_php
module G_ = AST_generic_

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

type classmember =
  | ConstantDef of A.constant_def
  | ClassVar of A.class_var
  | MethodDef of A.method_def
  | UseTrait of A.class_name
  | EnumCase of A.class_var (* TODO add enum case to AST *)

let todo (env : env) _ = failwith "not implemented"
let map_name (env : env) tok : A.name = [ _str env tok ]

let rec _split_classmembers env members constants variables methods uses =
  match members with
  | [] -> (constants, variables, methods, uses)
  | hd :: rest -> (
      match hd with
      | ConstantDef c -> (constants @ [ c ], variables, methods, uses)
      | ClassVar c -> (constants, variables @ [ c ], methods, uses)
      | MethodDef m -> (constants, variables, methods @ [ m ], uses)
      | UseTrait u -> (constants, variables, methods, uses @ [ u ])
      | EnumCase c -> (constants, variables @ [ c ], methods, uses))

let split_classmembers env members = _split_classmembers env members [] [] [] []

let map_ref_expr (env : env) tok expr : A.expr =
  match tok with
  | Some tok -> (* "&" *) A.Ref (token env tok, expr)
  | None -> expr

let map_empty_block (env : env) semi = A.Block (Parse_info.fake_bracket semi [])

let map_empty_statement_to_semicolon env empty =
  match empty with
  | `Empty_stmt t -> token env t
  | _ -> failwith "not an empty statement"

let stmt1 xs =
  match xs with
  | [] -> A.Block (Parse_info.fake_bracket Parse_info.unsafe_sc [])
  | [ st ] -> st
  | xs -> A.Block (Parse_info.fake_bracket Parse_info.unsafe_sc xs)

let fake_call_to_builtin (env : env) tok args =
  let str, tok = tok in
  A.Call (A.Id [ (A.builtin str, tok) ], Parse_info.fake_bracket tok args)

let rec chain_else_if (env : env) ifelses (else_ : A.stmt) : A.stmt =
  match ifelses with
  | [] -> else_
  | (tok, expr, stmt) :: tail ->
      A.If (tok, expr, stmt, chain_else_if env tail else_)

let map_primitive_type (env : env) (x : CST.primitive_type) : A.hint_type =
  match x with
  | `Array tok -> (* "array" *) HintArray (token env tok)
  | `Call tok -> (* "callable" *) Hint (map_name env tok)
  | `Iter tok -> (* "iterable" *) Hint (map_name env tok)
  | `Bool tok -> (* "bool" *) Hint (map_name env tok)
  | `Float tok -> (* "float" *) Hint (map_name env tok)
  | `Int tok -> (* "int" *) Hint (map_name env tok)
  | `Str tok -> (* "string" *) Hint (map_name env tok)
  | `Void tok -> (* "void" *) Hint (map_name env tok)
  | `Mixed tok -> (* "mixed" *) Hint (map_name env tok)
  | `Static tok -> (* "static" *) Hint (map_name env tok)
  | `False tok -> (* "false" *) Hint (map_name env tok)
  | `Null tok -> (* "null" *) Hint (map_name env tok)

let map_cast_type (env : env) (x : CST.cast_type) =
  match x with
  | `Array tok -> (* "array" *) (A.ArrayTy, token env tok)
  | `Bin tok -> (* "binary" *) (A.StringTy, token env tok)
  | `Bool_c506ff1 tok -> (* "bool" *) (A.BoolTy, token env tok)
  | `Bool_84e2c64 tok -> (* "boolean" *) (A.BoolTy, token env tok)
  | `Double tok -> (* "double" *) (A.DoubleTy, token env tok)
  | `Int_fa7153f tok -> (* "int" *) (A.IntTy, token env tok)
  | `Int_157db7d tok -> (* "integer" *) (A.IntTy, token env tok)
  | `Float tok -> (* "float" *) (A.DoubleTy, token env tok)
  | `Obj tok -> (* "object" *) (A.ObjectTy, token env tok)
  | `Real tok -> (* "real" *) (A.DoubleTy, token env tok)
  | `Str tok -> (* "string" *) (A.StringTy, token env tok)
  | `Unset tok -> (* "unset" *) (A.ObjectTy, token env tok)

let map_anon_choice_COLON_5102e09 (env : env)
    (x : CST.anon_choice_COLON_5102e09) =
  match x with
  | `COLON tok -> (* ":" *) token env tok
  | `SEMI tok -> (* ";" *) token env tok

let map_text (env : env) (xs : CST.text) =
  Common.map
    (fun x ->
      match x with
      | `LT tok -> (* < *) token env tok
      | `Pat_b91d208 tok -> (* pattern [^\s<][^<]* *) token env tok)
    xs

let map_namespace_name (env : env) ((v1, v2) : CST.namespace_name) =
  let v1 =
    (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) _str env v1
  in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "\\" *) token env v1 in
        let v2 =
          (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
          _str env v2
        in
        v2)
      v2
  in
  v1 :: v2

let map_named_label_statement (env : env) ((v1, v2) : CST.named_label_statement)
    =
  let v1 =
    (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) _str env v1
  in
  let v2 = (* ":" *) token env v2 in
  A.Label (v1, v2, map_empty_block env v2)

let map_variable_name (env : env) ((v1, v2) : CST.variable_name) : A.var =
  let v1 = (* "$" *) token env v1 in
  let v2str, v2tok = _str env v2 in
  let combined = Parse_info.combine_infos v1 [ v2tok ] in
  ("$" ^ v2str, combined)

let map_namespace_aliasing_clause (env : env)
    ((v1, v2) : CST.namespace_aliasing_clause) : A.ident =
  let v1 = (* pattern [aA][sS] *) token env v1 in
  let v2 =
    (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
    _str env v2
  in
  v2

let map_visibility_modifier (env : env) (x : CST.visibility_modifier) =
  match x with
  | `Pat_a9304a9 tok ->
      (* pattern [pP][uU][bB][lL][iI][cC] *) (A.Public, token env tok)
  | `Pat_954cb76 tok ->
      (* pattern [pP][rR][oO][tT][eE][cC][tT][eE][dD] *)
      (A.Protected, token env tok)
  | `Pat_1206b1e tok ->
      (* pattern [pP][rR][iI][vV][aA][tT][eE] *) (A.Private, token env tok)

let map_string__ (env : env) (x : CST.string__) =
  match x with
  | `Str tok -> (* string *) A.String (_str env tok)
  | `Here tok -> (* heredoc *) A.String (_str env tok)

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
    : A.name =
  match x with
  | `BSLASH tok -> (* "\\" *) []
  | `Opt_BSLASH_name_name_BSLASH (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some tok -> (* "\\" *) Some (map_name env tok)
        | None -> None
      in
      let v2 = map_namespace_name env v2 in
      let v3 = (* "\\" *) token env v3 in
      v2
  | `Pat_1e9d49b_BSLASH (v1, v2) ->
      let v1 =
        (* pattern [nN][aA][mM][eE][sS][pP][aA][cC][eE] *) token env v1
      in
      let v2 = (* "\\" *) token env v2 in
      []
  | `Pat_1e9d49b_opt_BSLASH_name_name_BSLASH (v1, v2, v3, v4) ->
      let v1 =
        (* pattern [nN][aA][mM][eE][sS][pP][aA][cC][eE] *) token env v1
      in
      let v2 =
        match v2 with
        | Some tok -> (* "\\" *) Some (token env tok)
        | None -> None
      in
      let v3 = map_namespace_name env v3 in
      let v4 = (* "\\" *) token env v4 in
      v3

let map_anonymous_function_use_clause (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.anonymous_function_use_clause) :
    (bool (* is_ref *) * A.var) list =
  let v1 = (* pattern [uU][sS][eE] *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = (* "&" *) Option.is_some v3 in
  let v4 = map_variable_name env v4 in
  let v5 =
    Common.map
      (fun (v1, v2, v3) ->
        let v1 = (* "," *) token env v1 in
        let v2 = (* "&" *) Option.is_some v2 in
        let v3 = map_variable_name env v3 in
        (v2, v3))
      v5
  in
  let v6 =
    match v6 with
    | Some tok -> (* "," *) Some (token env tok)
    | None -> None
  in
  let v7 = (* ")" *) token env v7 in
  (v3, v4) :: v5

let map_integer env tok =
  let value, tok = _str env tok in
  let value = int_of_string value in
  A.Int (Some value, tok)

let map_literal (env : env) (x : CST.literal) : A.expr =
  match x with
  | `Int tok ->
      (* integer *)
      map_integer env tok
  | `Float tok ->
      (* pattern \d*(_\d+)*((\.\d*(_\d+)*\
         )?([eE][\+-]?\d+(_\d+)*\
         )|(\.\d\d*(_\d+)*\
         )([eE][\+-]?\d+(_\d+)*\
         )?) *)
      let value, tok = _str env tok in
      let value = float_of_string value in
      A.Double (Some value, tok)
  | `Str_ x -> map_string__ env x
  | `Bool tok ->
      (* pattern [Tt][Rr][Uu][Ee]|[Ff][Aa][Ll][Ss][Ee] *)
      (* TODO Bool should have its own AST node *)
      Id [ _str env tok ]
  | `Null tok ->
      (* pattern [nN][uU][lL][lL] *)
      (* TODO Null should have its own AST node *)
      Id [ _str env tok ]

let map_namespace_use_group_clause (env : env)
    ((v1, v2, v3) : CST.namespace_use_group_clause) =
  let v1 =
    match v1 with
    | Some x -> Some (map_anon_choice_pat_174c3a5_81b85de env x)
    | None -> None
  in
  let v2 = map_namespace_name env v2 in
  let v3 =
    match v3 with
    | Some x -> Some (map_namespace_aliasing_clause env x)
    | None -> None
  in
  (v2, v3)

let map_modifier (env : env) (x : CST.modifier) : A.modifier =
  match x with
  | `Var_modi tok ->
      (* pattern [vV][aA][rR] *) todo env tok (* TODO add to AST *)
  | `Visi_modi x -> map_visibility_modifier env x
  | `Static_modi tok ->
      (* pattern [sS][tT][aA][tT][iI][cC] *) (A.Static, token env tok)
  | `Final_modi tok ->
      (* pattern [fF][iI][nN][aA][lL] *) (A.Final, token env tok)
  | `Abst_modi tok ->
      (* pattern [aA][bB][sS][tT][rR][aA][cC][tT] *) (A.Abstract, token env tok)

let map_relative_scope (env : env) (x : CST.relative_scope) =
  match x with
  | `Self tok -> (* "self" *) A.IdSpecial (A.Self, token env tok)
  | `Parent tok -> (* "parent" *) A.IdSpecial (A.Parent, token env tok)
  | `Pat_068a1b3 tok ->
      (* pattern [sS][tT][aA][tT][iI][cC] *) A.Id (map_name env tok)

let map_reserved_identifier (env : env) (x : CST.reserved_identifier) =
  match x with
  | `Self tok -> (* "self" *) A.IdSpecial (A.Self, token env tok)
  | `Parent tok -> (* "parent" *) A.IdSpecial (A.Parent, token env tok)
  | `Pat_068a1b3 tok ->
      (* pattern [sS][tT][aA][tT][iI][cC] *) A.Id (map_name env tok)

let map_reserved_identifier_ident (env : env) (x : CST.reserved_identifier) =
  match x with
  | `Self tok -> (* "self" *) _str env tok
  | `Parent tok -> (* "parent" *) _str env tok
  | `Pat_068a1b3 tok -> (* pattern [sS][tT][aA][tT][iI][cC] *) _str env tok

let map_qualified_name (env : env) ((v1, v2) : CST.qualified_name) =
  let v1 = map_namespace_name_as_prefix env v1 in
  let v2 =
    (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
    map_name env v2
  in
  v1 @ v2

let map_declare_directive (env : env) ((v1, v2, v3) : CST.declare_directive) =
  let v1 =
    match v1 with
    | `Ticks tok -> (* "ticks" *) A.Id (map_name env tok)
    | `Enco tok -> (* "encoding" *) A.Id (map_name env tok)
    | `Strict_types tok -> (* "strict_types" *) A.Id (map_name env tok)
  in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_literal env v3 in
  A.Assign (v1, v2, v3)

let map_namespace_use_group (env : env)
    ((v1, v2, v3, v4) : CST.namespace_use_group) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_namespace_use_group_clause env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_namespace_use_group_clause env v2 in
        v2)
      v3
  in
  let v4 = (* "}" *) token env v4 in
  v2 :: v3

let map_anon_choice_name_9dd129a (env : env) (x : CST.anon_choice_name_9dd129a)
    : A.ident =
  match x with
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      _str env tok
  | `Rese_id x -> map_reserved_identifier_ident env x

let map_named_type (env : env) (x : CST.named_type) : A.hint_type =
  match x with
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      Hint (map_name env tok)
  | `Qual_name x -> Hint (map_qualified_name env x)

let map_anon_choice_name_062e4f2 (env : env) (x : CST.anon_choice_name_062e4f2)
    : A.name =
  match x with
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      map_name env tok
  | `Rese_id x -> [ map_reserved_identifier_ident env x ]
  | `Qual_name x -> map_qualified_name env x

let map_type_list (env : env) ((v1, v2) : CST.type_list) : A.hint_type list =
  let v1 = map_named_type env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "|" *) token env v1 in
        let v2 = map_named_type env v2 in
        v2)
      v2
  in
  v1 :: v2

let map_base_clause (env : env) ((v1, v2, v3) : CST.base_clause) =
  let v1 = (* pattern [eE][xX][tT][eE][nN][dD][sS] *) token env v1 in
  let v2 = map_anon_choice_name_062e4f2 env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_name_062e4f2 env v2 in
        v2)
      v3
  in
  Common.map (fun c -> A.Hint c) (v2 :: v3)

let map_class_interface_clause (env : env)
    ((v1, v2, v3) : CST.class_interface_clause) : A.class_name list =
  let v1 =
    (* pattern [iI][mM][pP][lL][eE][mM][eE][nN][tT][sS] *) token env v1
  in
  let v2 = A.Hint (map_anon_choice_name_062e4f2 env v2) in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = A.Hint (map_anon_choice_name_062e4f2 env v2) in
        v2)
      v3
  in
  v2 :: v3

let map_namespace_use_clause (env : env) ((v1, v2) : CST.namespace_use_clause) =
  let v1 = map_anon_choice_name_062e4f2 env v1 in
  let v2 =
    match v2 with
    | Some x -> Some (map_namespace_aliasing_clause env x)
    | None -> None
  in
  (v1, v2)

let map_types (env : env) (x : CST.types) =
  match x with
  | `Opt_type (v1, v2) ->
      let v1 = (* "?" *) token env v1 in
      let v2 =
        match v2 with
        | `Named_type x -> map_named_type env x
        | `Prim_type x -> map_primitive_type env x
      in
      A.HintQuestion (v1, v2)
  | `Named_type x -> map_named_type env x
  | `Prim_type x -> map_primitive_type env x

let map_union_type (env : env) ((v1, v2) : CST.union_type) =
  let v1 = map_types env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "|" *) token env v1 in
        let v2 = map_types env v2 in
        v2)
      v2
  in
  match v2 with
  | [] -> v1
  | _ -> HintTuple (Parse_info.fake_bracket Parse_info.unsafe_sc (v1 :: v2))

let map_type_ (env : env) (x : CST.type_) = map_union_type env x

let map_return_type (env : env) ((v1, v2) : CST.return_type) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_type_ env v2 in
  v2

let rec map_anon_array_elem_init_rep_COMMA_array_elem_init_1dad3d4 (env : env)
    ((v1, v2) : CST.anon_array_elem_init_rep_COMMA_array_elem_init_1dad3d4) =
  let v1 = map_array_element_initializer env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_array_element_initializer env v2 in
        v2)
      v2
  in
  v1 :: v2

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
      let v4 = Common.map (map_statement env) v4 in
      A.Case (v1, v2, v4)
  | `Defa_stmt (v1, v2, v3) ->
      let v1 = (* pattern [dD][eE][fF][aA][uU][lL][tT] *) token env v1 in
      let v2 = map_anon_choice_COLON_5102e09 env v2 in
      let v3 = Common.map (map_statement env) v3 in
      A.Default (v1, v3)

and map_anon_choice_choice_array_dest_abfb170 (env : env)
    (x : CST.anon_choice_choice_array_dest_abfb170) =
  match x with
  | `Choice_array_dest x -> map_anon_choice_array_dest_08f4c18 env x
  | `Exp_EQGT_choice_array_dest (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_anon_choice_array_dest_08f4c18 env v3 in
      A.Arrow (v1, v2, v3)

and map_anon_choice_choice_list_dest_c865322 (env : env)
    (x : CST.anon_choice_choice_list_dest_c865322) =
  match x with
  | `Choice_list_dest x -> map_anon_choice_list_dest_bb41c20 env x
  | `Exp_EQGT_choice_list_dest (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_anon_choice_list_dest_bb41c20 env v3 in
      A.Arrow (v1, v2, v3)

and map_anon_choice_class_cst_access_exp_18f5288 (env : env)
    (x : CST.anon_choice_class_cst_access_exp_18f5288) =
  match x with
  | `Class_cst_access_exp x -> map_class_constant_access_expression env x
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      A.Id (map_name env tok)

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
        | None -> []
      in
      let v2 =
        match v2 with
        | Some x -> Some (map_type_ env x)
        | None -> None
      in
      let v3 =
        match v3 with
        | Some tok -> (* "&" *) Some (token env tok)
        | None -> None
      in
      let v4 = map_variable_name env v4 in
      let v5 =
        match v5 with
        | Some x -> Some (map_property_initializer env x)
        | None -> None
      in
      A.ParamClassic
        {
          p_type = v2;
          p_ref = v3;
          p_name = v4;
          p_default = v5;
          p_attrs = v1;
          p_variadic = None;
        }
  | `Vari_param (v1, v2, v3, v4, v5) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> []
      in
      let v2 =
        match v2 with
        | Some x -> Some (map_type_ env x)
        | None -> None
      in
      let v3 =
        match v3 with
        | Some tok -> (* "&" *) Some (token env tok)
        | None -> None
      in
      let v4 = (* "..." *) token env v4 in
      let v5 = map_variable_name env v5 in
      A.ParamClassic
        {
          p_type = v2;
          p_ref = v3;
          p_name = v5;
          p_default = None;
          p_attrs = v1;
          p_variadic = Some v4;
        }
  | `Prop_prom_param (v1, v2, v3, v4) ->
      let v1_todo = map_visibility_modifier env v1 in
      let v2 =
        match v2 with
        | Some x -> Some (map_type_ env x)
        | None -> None
      in
      let v3 = map_variable_name env v3 in
      let v4 =
        match v4 with
        | Some x -> Some (map_property_initializer env x)
        | None -> None
      in
      A.ParamClassic
        {
          p_type = v2;
          p_ref = None;
          p_name = v3;
          p_default = v4;
          p_attrs = [];
          p_variadic = None;
        }

and map_argument (env : env) ((v1, v2) : CST.argument) =
  let v1_todo =
    match v1 with
    | Some x -> Some (map_named_label_statement env x)
    | None -> None
  in
  let v2 =
    match v2 with
    | `Vari_unpa x -> map_variadic_unpacking env x
    | `Exp x -> map_expression env x
  in
  v2

and map_arguments (env : env) ((v1, v2, v3, v4) : CST.arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_argument env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let v1 = (* "," *) token env v1 in
              let v2 = map_argument env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 =
    match v3 with
    | Some tok -> (* "," *) Some (token env tok)
    | None -> None
  in
  let v4 = (* ")" *) token env v4 in
  (v1, v2, v4)

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
        | None -> []
      in
      let v4 =
        match v4 with
        | Some tok -> (* "," *) Some (token env tok)
        | None -> None
      in
      let v5 = (* ")" *) token env v5 in
      A.ConsArray (v2, v3, v5)
  | `LBRACK_opt_array_elem_init_rep_COMMA_array_elem_init_opt_COMMA_RBRACK
      (v1, v2, v3, v4) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        match v2 with
        | Some x ->
            map_anon_array_elem_init_rep_COMMA_array_elem_init_1dad3d4 env x
        | None -> []
      in
      let v3 =
        match v3 with
        | Some tok -> (* "," *) Some (token env tok)
        | None -> None
      in
      let v4 = (* "]" *) token env v4 in
      A.ConsArray (v1, v2, v4)

and map_array_destructing (env : env) ((v1, v2, v3, v4) : CST.array_destructing)
    =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_anon_choice_choice_array_dest_abfb170 env x
    | None -> A.Id [ ("", Parse_info.fake_info v1 "") ]
  in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 =
          match v2 with
          | Some x -> map_anon_choice_choice_array_dest_abfb170 env x
          | None -> A.Id [ ("", Parse_info.fake_info v1 "") ]
        in
        v2)
      v3
  in
  let v4 = (* "]" *) token env v4 in
  A.ConsArray (v1, v2 :: v3, v4)

and map_array_element_initializer (env : env)
    (x : CST.array_element_initializer) =
  match x with
  | `Opt_AMP_exp (v1, v2) ->
      let v2 = map_expression env v2 in
      map_ref_expr env v1 v2
  | `Exp_EQGT_opt_AMP_exp (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v4 = map_expression env v4 in
      let v4 = map_ref_expr env v3 v4 in
      A.Arrow (v1, v2, v4)
  | `Vari_unpa x -> map_variadic_unpacking env x

and map_attribute (env : env) ((v1, v2) : CST.attribute) : A.attribute =
  let v1 = map_anon_choice_name_062e4f2 env v1 in
  match v2 with
  | Some x ->
      let args = map_arguments env x in
      A.Call (A.Id v1, args)
  | None -> A.Id v1

and map_attribute_list (env : env) (xs : CST.attribute_list) : A.attribute list
    =
  List.concat_map
    (fun (v1, v2, v3, v4) ->
      let v1 = (* "#[" *) token env v1 in
      let v2 = map_attribute env v2 in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_attribute env v2 in
            v2)
          v3
      in
      let v4 = (* "]" *) token env v4 in
      v2 :: v3)
    xs

and map_binary_expression (env : env) (x : CST.binary_expression) =
  match x with
  | `Un_exp_pat_d58874b_choice_qual_name (v1, v2, v3) ->
      let v1 = map_unary_expression env v1 in
      let v2 =
        (* pattern [iI][nN][sS][tT][aA][nN][cC][eE][oO][fF] *)
        (A.ArithOp G_.Is, token env v2)
      in
      let v3 = map_class_type_designator env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_QMARKQMARK_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "??" *) (A.ArithOp G_.Nullish, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_pat_e0610ac_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* pattern and|AND *) (A.ArithOp G_.And, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_pat_48a4c46_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* pattern or|OR *) (A.ArithOp G_.Or, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_pat_f398476_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* pattern xor|XOR *) (A.ArithOp G_.Xor, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) (A.ArithOp G_.Or, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) (A.ArithOp G_.And, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) (A.ArithOp G_.BitOr, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) (A.ArithOp G_.BitXor, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) (A.ArithOp G_.BitAnd, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) (A.ArithOp G_.Eq, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "!=" *) (A.ArithOp G_.NotEq, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_LTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<>" *) (A.ArithOp G_.NotEq, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_EQEQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "===" *) (A.ArithOp G_.PhysEq, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_BANGEQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "!==" *) (A.ArithOp G_.NotPhysEq, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) (A.ArithOp G_.Lt, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) (A.ArithOp G_.Gt, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) (A.ArithOp G_.LtE, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) (A.ArithOp G_.GtE, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_LTEQGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<=>" *) (A.ArithOp G_.Cmp, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) (A.ArithOp G_.LSL, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) (A.ArithOp G_.LSR, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) (A.ArithOp G_.Plus, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) (A.ArithOp G_.Minus, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_DOT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "." *) (A.ArithOp G_.Concat, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) (A.ArithOp G_.Mult, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) (A.ArithOp G_.Div, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) (A.ArithOp G_.Mod, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)

and map_callable_expression (env : env) (x : CST.callable_expression) : A.expr =
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
      A.Call (A.Obj_get (v1, v2, v3), v4)
  | `Null_member_call_exp (v1, v2, v3, v4) ->
      let v1 = map_dereferencable_expression env v1 in
      let v2 = (* "?->" *) token env v2 in
      (* TODO add nullsafe operator to AST *)
      let v3 = map_member_name env v3 in
      let v4 = map_arguments env v4 in
      A.Call (A.Obj_get (v1, v2, v3), v4)
  | `Scoped_call_exp (v1, v2, v3, v4) ->
      let v1 = map_scope_resolution_qualifier env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_member_name env v3 in
      let v4 = map_arguments env v4 in
      A.Call (A.Class_get (v1, v2, v3), v4)
  | `Func_call_exp (v1, v2) ->
      let v1 =
        match v1 with
        | `Name tok ->
            (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
            A.Id (map_name env tok)
        | `Rese_id x -> map_reserved_identifier env x
        | `Qual_name x -> A.Id (map_qualified_name env x)
        | `Choice_choice_choice_dyna_var_name x -> map_callable_expression env x
      in
      let v2 = map_arguments env v2 in
      A.Call (v1, v2)

and map_catch_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.catch_clause) :
    A.catch =
  let v1 = (* pattern [cC][aA][tT][cC][hH] *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_type_list env v3 in
  let v4 =
    match v4 with
    | Some x -> map_variable_name env x
    | None -> ("", Parse_info.fake_info v2 "")
  in
  let v5 = (* ")" *) token env v5 in
  let v6 = map_compound_statement env v6 in
  let ht = A.HintTuple (v2, v3, v5) in
  (v1, ht, v4, v6)

and map_class_constant_access_expression (env : env)
    ((v1, v2, v3) : CST.class_constant_access_expression) : A.expr =
  let v1 = map_scope_resolution_qualifier env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 = map_anon_choice_name_9dd129a env v3 in
  A.Class_get (v1, v2, A.Id [ v3 ])

and map_class_type_designator (env : env) (x : CST.class_type_designator) =
  match x with
  | `Qual_name x -> A.Id (map_qualified_name env x)
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      A.Id (map_name env tok)
  | `Rese_id x -> map_reserved_identifier env x
  | `Subs_exp x -> map_subscript_expression env x
  | `Member_access_exp x -> map_member_access_expression env x
  | `Null_member_access_exp x -> map_nullsafe_member_access_expression env x
  | `Scoped_prop_access_exp x -> map_scoped_property_access_expression env x
  | `Choice_dyna_var_name x -> map_variable_name_ env x

and map_clone_expression (env : env) ((v1, v2) : CST.clone_expression) =
  let v1 = (* "clone" *) _str env v1 in
  let v2 = map_primary_expression env v2 in
  fake_call_to_builtin env v1 [ v2 ]

and map_colon_block (env : env) ((v1, v2) : CST.colon_block) =
  let v1 = (* ":" *) token env v1 in
  let v2 = Common.map (map_statement env) v2 in
  A.Block (v1, v2, v1)

and map_compound_statement_ (env : env) ((v1, v2, v3) : CST.compound_statement)
    =
  let v1 = (* "{" *) token env v1 in
  let v2 = Common.map (map_statement env) v2 in
  let v3 = (* "}" *) token env v3 in
  (v1, v2, v3)

and map_compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) =
  A.Block (map_compound_statement_ env (v1, v2, v3))

and map_const_declaration (env : env) (x : CST.const_declaration) :
    classmember list =
  let consts = map_const_declaration_ env x in
  Common.map (fun c -> ConstantDef c) consts

and map_const_declaration_ (env : env)
    ((v1, v2, v3, v4, v5) : CST.const_declaration_) =
  let v1 =
    match v1 with
    | Some x -> [ map_visibility_modifier env x ]
    | None -> []
  in
  let v2 = (* pattern [cC][oO][nN][sS][tT] *) token env v2 in
  let v3 = map_const_element env v3 in
  let v4 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_const_element env v2 in
        v2)
      v4
  in
  let v5 = map_semicolon env v5 in
  Common.map
    (fun (name, expr) ->
      { A.cst_tok = v2; A.cst_name = name; A.cst_body = expr })
    (v3 :: v4)

and map_const_element (env : env) ((v1, v2, v3) : CST.const_element) =
  let v1 = map_anon_choice_name_9dd129a env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_expression env v3 in
  (v1, v3)

and map_declaration_list (env : env) ((v1, v2, v3) : CST.declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 = List.concat_map (map_member_declaration env) v2 in
  let v3 = (* "}" *) token env v3 in
  (v1, v2, v3)

and map_dereferencable_expression (env : env)
    (x : CST.dereferencable_expression) : A.expr =
  match x with
  | `Choice_cast_var x -> map_variable env x
  | `Class_cst_access_exp x -> map_class_constant_access_expression env x
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Array_crea_exp x -> map_array_creation_expression env x
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      A.Id (map_name env tok)
  | `Rese_id x -> map_reserved_identifier env x
  | `Qual_name x -> A.Id (map_qualified_name env x)
  | `Str_ x -> map_string__ env x

and map_dynamic_variable_name (env : env) (x : CST.dynamic_variable_name) =
  match x with
  | `DOLLAR_choice_dyna_var_name (v1, v2) ->
      let v1 = (* "$" *) token env v1 in
      let v2 = map_variable_name_ env v2 in
      A.Call
        (A.Id [ (A.builtin "eval_var", v1) ], Parse_info.fake_bracket v1 [ v2 ])
  | `DOLLAR_LCURL_exp_RCURL (v1, v2, v3, v4) ->
      let v1 = (* "$" *) token env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "}" *) token env v4 in
      A.Call (A.Id [ (A.builtin "eval_var", v1) ], (v2, [ v3 ], v4))

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = (* pattern [eE][lL][sS][eE] *) token env v1 in
  let v2 = map_statement env v2 in
  v2

and map_else_clause_2 (env : env) ((v1, v2) : CST.else_clause_2) =
  let v1 = (* pattern [eE][lL][sS][eE] *) token env v1 in
  let v2 = map_colon_block env v2 in
  v2

and map_else_if_clause (env : env) ((v1, v2, v3) : CST.else_if_clause) =
  let v1 = (* pattern [eE][lL][sS][eE][iI][fF] *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_statement env v3 in
  (v1, v2, v3)

and map_else_if_clause_2 (env : env) ((v1, v2, v3) : CST.else_if_clause_2) =
  let v1 = (* pattern [eE][lL][sS][eE][iI][fF] *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_colon_block env v3 in
  (v1, v2, v3)

and map_enum_declaration_list (env : env)
    ((v1, v2, v3) : CST.enum_declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 = List.concat_map (map_enum_member_declaration env) v2 in
  let v3 = (* "}" *) token env v3 in
  (v1, v2, v3)

and map_enum_member_declaration (env : env) (x : CST.enum_member_declaration) :
    classmember list =
  match x with
  | `Enum_case (v1, v2, v3, v4, v5) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> []
      in
      let v2 = (* "case" *) token env v2 in
      let v3 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
        _str env v3
      in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            let v1 = (* "=" *) token env v1 in
            let v2 =
              match v2 with
              | `Str tok ->
                  (* string *)
                  ( Some (A.Hint [ ("string", token env tok) ]),
                    Some (A.String (_str env tok)) )
              | `Int tok ->
                  (* integer *)
                  ( Some (A.Hint [ ("int", token env tok) ]),
                    Some (map_integer env tok) )
            in
            v2
        | None -> (None, None)
      in
      let v5 = map_semicolon env v5 in
      let type_, value = v4 in
      [
        EnumCase
          { cv_name = v3; cv_type = type_; cv_value = value; cv_modifiers = [] };
      ]
  | `Meth_decl x -> [ map_method_declaration env x ]
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
  Binop (v1, (ArithOp G_.Pow, v2), v3)

and map_expression (env : env) (x : CST.expression) : A.expr =
  match x with
  | `Cond_exp (v1, v2, v3, v4, v5) -> (
      let v1 = map_expression env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      match v3 with
      | Some e -> A.CondExpr (v1, e, v5)
      | None ->
          let elvis =
            (A.ArithOp G_.Elvis, Parse_info.combine_infos v2 [ v4 ])
          in
          A.Binop (v1, elvis, v5))
  | `Match_exp (v1, v2, v3) ->
      let v1 = (* pattern [mM][aA][tT][cC][hH] *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_match_block env v3 in
      todo env (v1, v2, v3)
  | `Augm_assign_exp (v1, v2, v3) ->
      let v1 = map_variable env v1 in
      let v2 =
        match v2 with
        | `STARSTAREQ tok -> (* "**=" *) (A.ArithOp G_.Pow, token env tok)
        | `STAREQ tok -> (* "*=" *) (A.ArithOp G_.Mult, token env tok)
        | `SLASHEQ tok -> (* "/=" *) (A.ArithOp G_.Div, token env tok)
        | `PERCEQ tok -> (* "%=" *) (A.ArithOp G_.Mod, token env tok)
        | `PLUSEQ tok -> (* "+=" *) (A.ArithOp G_.Plus, token env tok)
        | `DASHEQ tok -> (* "-=" *) (A.ArithOp G_.Minus, token env tok)
        | `DOTEQ tok -> (* ".=" *) (A.ArithOp G_.Concat, token env tok)
        | `LTLTEQ tok -> (* "<<=" *) (A.ArithOp G_.LSL, token env tok)
        | `GTGTEQ tok -> (* ">>=" *) (A.ArithOp G_.LSR, token env tok)
        | `AMPEQ tok -> (* "&=" *) (A.ArithOp G_.BitAnd, token env tok)
        | `HATEQ tok -> (* "^=" *) (A.ArithOp G_.BitXor, token env tok)
        | `BAREQ tok -> (* "|=" *) (A.ArithOp G_.BitOr, token env tok)
        | `QMARKQMARKEQ tok -> (* "??=" *) (A.ArithOp G_.Nullish, token env tok)
      in
      let v3 = map_expression env v3 in
      A.AssignOp (v1, v2, v3)
  | `Assign_exp (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | `Choice_cast_var x -> map_variable env x
        | `List_lit x -> map_list_literal env x
      in
      let v2 = (* "=" *) token env v2 in
      let v4 = map_expression env v4 in
      let v4 = map_ref_expr env v3 v4 in
      A.Assign (v1, v2, v4)
  | `Yield_exp (v1, v2) ->
      let v1 = (* "yield" *) _str env v1 in
      let v2 =
        match v2 with
        | Some x -> (
            match x with
            | `Array_elem_init x -> [ map_array_element_initializer env x ]
            | `From_exp (v1, v2) ->
                let v1 = (* "from" *) token env v1 in
                (* TODO handle `yield from` *)
                let v2 = map_expression env v2 in
                [ v2 ])
        | None -> []
      in
      fake_call_to_builtin env v1 v2
  | `Un_exp x -> map_unary_expression env x
  | `Bin_exp x -> map_binary_expression env x
  | `Incl_exp (v1, v2) ->
      let v1 = (* pattern [iI][nN][cC][lL][uU][dD][eE] *) _str env v1 in
      let v2 = map_expression env v2 in
      fake_call_to_builtin env v1 [ v2 ]
  | `Incl_once_exp (v1, v2) ->
      let v1 =
        (* pattern [iI][nN][cC][lL][uU][dD][eE][__][oO][nN][cC][eE] *)
        _str env v1
      in
      let v2 = map_expression env v2 in
      fake_call_to_builtin env v1 [ v2 ]
  | `Requ_exp (v1, v2) ->
      let v1 = (* pattern [rR][eE][qQ][uU][iI][rR][eE] *) _str env v1 in
      let v2 = map_expression env v2 in
      fake_call_to_builtin env v1 [ v2 ]
  | `Requ_once_exp (v1, v2) ->
      let v1 =
        (* pattern [rR][eE][qQ][uU][iI][rR][eE][__][oO][nN][cC][eE] *)
        _str env v1
      in
      let v2 = map_expression env v2 in
      fake_call_to_builtin env v1 [ v2 ]

and map_expressions (env : env) (x : CST.expressions) : A.expr list =
  match x with
  | `Exp x -> [ map_expression env x ]
  | `Seq_exp x -> map_sequence_expression env x

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = (* pattern [fF][iI][nN][aA][lL][lL][yY] *) token env v1 in
  let v2 = map_compound_statement env v2 in
  (v1, v2)

and split_catch_finally (env : env) cfs catches finallies =
  match cfs with
  | [] -> (catches, finallies)
  | cf :: tail -> (
      let catches, finallies = split_catch_finally env tail catches finallies in
      match cf with
      | `Catch_clause x ->
          let c = map_catch_clause env x in
          (c :: catches, finallies)
      | `Fina_clause x ->
          let catches, finallies =
            split_catch_finally env tail catches finallies
          in
          let f = map_finally_clause env x in
          (catches, f :: finallies))

and map_foreach_pair (env : env) ((v1, v2, v3) : CST.foreach_pair) =
  let v1 = map_expression env v1 in
  let v2 = (* "=>" *) token env v2 in
  let v3 = map_foreach_value env v3 in
  A.Arrow (v1, v2, v3)

and map_foreach_value (env : env) (x : CST.foreach_value) =
  match x with
  | `Opt_AMP_exp (v1, v2) ->
      let v2 = map_expression env v2 in
      map_ref_expr env v1 v2
  | `List_lit x -> map_list_literal env x

and map_formal_parameters (env : env) ((v1, v2, v3, v4) : CST.formal_parameters)
    : A.parameter list =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_simple_param_5af5eb3 env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let v1 = (* "," *) token env v1 in
              let v2 = map_anon_choice_simple_param_5af5eb3 env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 =
    match v3 with
    | Some tok -> (* "," *) Some (token env tok)
    | None -> None
  in
  let v4 = (* ")" *) token env v4 in
  v2

and map_function_definition_header (env : env)
    ((v1, v2, v3, v4, v5) : CST.function_definition_header) =
  let v1 = (* pattern [fF][uU][nN][cC][tT][iI][oO][nN] *) token env v1 in
  let v2 = Option.is_some v2 in
  let v3 = map_anon_choice_name_9dd129a env v3 in
  let v4 = map_formal_parameters env v4 in
  let v5 =
    match v5 with
    | Some x -> Some (map_return_type env x)
    | None -> None
  in
  (v1, v2, v3, v4, v5)

and map_list_destructing (env : env)
    ((v1, v2, v3, v4, v5) : CST.list_destructing) =
  let v1 = (* "list" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    match v3 with
    | Some x -> map_anon_choice_choice_list_dest_c865322 env x
    | None -> A.Id [ ("", Parse_info.fake_info v2 "") ]
  in
  let v4 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 =
          match v2 with
          | Some x -> map_anon_choice_choice_list_dest_c865322 env x
          | None -> A.Id [ ("", Parse_info.fake_info v1 "") ]
        in
        v2)
      v4
  in
  let v5 = (* ")" *) token env v5 in
  A.List (v2, v3 :: v4, v5)

and map_list_literal (env : env) (x : CST.list_literal) =
  match x with
  | `List_dest x -> map_list_destructing env x
  | `Array_dest x -> map_array_destructing env x

and map_match_block (env : env) ((v1, v2, v3, v4, v5) : CST.match_block) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_anon_choice_match_cond_exp_d891119 env v2 in
  let v3 =
    Common.map
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
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_expression env v2 in
        v2)
      v2
  in
  v1 :: v2

and map_member_access_expression (env : env)
    ((v1, v2, v3) : CST.member_access_expression) =
  let v1 = map_dereferencable_expression env v1 in
  let v2 = (* "->" *) token env v2 in
  let v3 = map_member_name env v3 in
  Obj_get (v1, v2, v3)

and map_member_declaration (env : env) (x : CST.member_declaration) :
    classmember list =
  match x with
  | `Class_const_decl (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> []
      in
      let v2 =
        match v2 with
        | Some tok ->
            (* pattern [fF][iI][nN][aA][lL] *) [ (A.Final, token env tok) ]
        | None -> []
      in
      let v3 = map_const_declaration env v3 in
      v3
  | `Prop_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> []
      in
      let v2 = Common.map (map_modifier env) v2 in
      let v3 =
        match v3 with
        | Some x -> Some (map_type_ env x)
        | None -> None
      in
      let v4 = map_property_element env v4 in
      let v5 =
        Common.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_property_element env v2 in
            v2)
          v5
      in
      let v6 = map_semicolon env v6 in
      Common.map
        (fun (name, value) ->
          ClassVar
            {
              A.cv_name = name;
              A.cv_type = v3;
              A.cv_value = value;
              A.cv_modifiers = v2;
            })
        (v4 :: v5)
  | `Meth_decl x -> [ map_method_declaration env x ]
  | `Use_decl x -> map_use_declaration env x

and map_member_name (env : env) (x : CST.member_name) =
  match x with
  | `Choice_rese_id x -> (
      match x with
      | `Rese_id x -> map_reserved_identifier env x
      | `Name tok ->
          (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
          A.Id (map_name env tok)
      | `Choice_dyna_var_name x -> map_variable_name_ env x)
  | `LCURL_exp_RCURL (v1, v2, v3) ->
      let v1 = (* "{" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "}" *) token env v3 in
      v2

and map_method_declaration (env : env)
    ((v1, v2, v3, v4) : CST.method_declaration) : classmember =
  let v1 =
    match v1 with
    | Some x -> map_attribute_list env x
    | None -> []
  in
  let v2 = Common.map (map_modifier env) v2 in
  let v3 = map_function_definition_header env v3 in
  let v4 =
    match v4 with
    | `Comp_stmt x -> map_compound_statement env x
    | `Choice_auto_semi x -> map_empty_block env (map_semicolon env x)
  in
  let tok, is_ref, name, params, return = v3 in
  MethodDef
    {
      A.f_name = name;
      A.f_kind = (Method, tok);
      A.f_params = params;
      A.f_return_type = return;
      A.f_ref = is_ref;
      A.m_modifiers = v2;
      A.f_attrs = v1;
      A.l_uses = [];
      A.f_body = v4;
    }

and map_nullsafe_member_access_expression (env : env)
    ((v1, v2, v3) : CST.nullsafe_member_access_expression) =
  let v1 = map_dereferencable_expression env v1 in
  let v2 = (* "?->" *) token env v2 in
  (* TODO add nullsafe operator to AST *)
  let v3 = map_member_name env v3 in
  A.Obj_get (v1, v2, v3)

and map_object_creation_expression (env : env)
    (x : CST.object_creation_expression) =
  match x with
  | `New_choice_qual_name_opt_args (v1, v2, v3) ->
      let v1 = (* "new" *) token env v1 in
      let v2 = map_class_type_designator env v2 in
      let v3 =
        match v3 with
        | Some x -> Parse_info.unbracket (map_arguments env x)
        | None -> []
      in
      A.New (v1, v2, v3)
  | `New_pat_a7a1629_opt_args_opt_base_clause_opt_class_inte_clause_decl_list
      (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "new" *) token env v1 in
      let v2 = (* pattern [cC][lL][aA][sS][sS] *) token env v2 in
      let v3 =
        match v3 with
        | Some x ->
            let _, args, _ = map_arguments env x in
            args
        | None -> []
      in
      let v4 =
        match v4 with
        | Some x -> Some (List.hd (map_base_clause env x))
        | None -> None
      in
      let v5 =
        match v5 with
        | Some x -> map_class_interface_clause env x
        | None -> []
      in
      let v6 = map_declaration_list env v6 in
      let opn, decls, cls = v6 in
      let consts, vars, methods, uses = split_classmembers env decls in
      A.NewAnonClass
        ( v2,
          v3,
          {
            c_name = ("", v1);
            c_kind = (A.Class, v1);
            c_extends = v4;
            c_implements = v5;
            c_uses = uses;
            c_enum_type = None;
            c_modifiers = [];
            c_attrs = [];
            c_constants = consts;
            c_variables = vars;
            c_methods = methods;
            c_braces = (opn, (), cls);
          } )

and map_parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) : A.expr =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  v2

and map_primary_expression (env : env) (x : CST.primary_expression) : A.expr =
  match x with
  | `Choice_cast_var x -> map_variable env x
  | `Choice_int x -> map_literal env x
  | `Class_cst_access_exp x -> map_class_constant_access_expression env x
  | `Qual_name x -> A.Id (map_qualified_name env x)
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      A.Id (map_name env tok)
  | `Array_crea_exp x -> map_array_creation_expression env x
  | `Print_intr (v1, v2) ->
      let v1 = (* "print" *) _str env v1 in
      let v2 = map_expression env v2 in
      fake_call_to_builtin env v1 [ v2 ]
  | `Anon_func_crea_exp (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        match v1 with
        | Some tok ->
            (* pattern [sS][tT][aA][tT][iI][cC] *) [ (A.Static, token env tok) ]
        | None -> []
      in
      let v2 = (* pattern [fF][uU][nN][cC][tT][iI][oO][nN] *) token env v2 in
      let v3 = (* "&" *) Option.is_some v3 in
      let v4 = map_formal_parameters env v4 in
      let v5 =
        match v5 with
        | Some x -> map_anonymous_function_use_clause env x
        | None -> []
      in
      let v6 =
        match v6 with
        | Some x -> Some (map_return_type env x)
        | None -> None
      in
      let v7 = map_compound_statement env v7 in
      A.Lambda
        {
          A.f_name = ("", v2);
          A.f_kind = (AnonLambda, v2);
          A.f_params = v4;
          A.f_return_type = v6;
          A.f_ref = v3;
          A.m_modifiers = v1;
          A.f_attrs = [];
          A.l_uses = v5;
          A.f_body = v7;
        }
  | `Arrow_func (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        match v1 with
        | Some tok ->
            (* pattern [sS][tT][aA][tT][iI][cC] *) [ (A.Static, token env tok) ]
        | None -> []
      in
      let v2 = (* pattern [fF][nN] *) token env v2 in
      let v3 = (* "&" *) Option.is_some v3 in
      let v4 = map_formal_parameters env v4 in
      let v5 =
        match v5 with
        | Some x -> Some (map_return_type env x)
        | None -> None
      in
      let v6 = (* "=>" *) token env v6 in
      let v7 = map_expression env v7 in
      A.Lambda
        {
          A.f_name = ("", v2);
          A.f_kind = (ShortLambda, v2);
          A.f_params = v4;
          A.f_return_type = v5;
          A.f_ref = v3;
          A.m_modifiers = v1;
          A.f_attrs = [];
          A.l_uses = [];
          A.f_body = Expr (v7, Parse_info.unsafe_sc);
        }
  | `Obj_crea_exp x -> map_object_creation_expression env x
  | `Update_exp x -> map_update_expression env x
  | `Shell_cmd_exp tok ->
      (* shell_command_expression *)
      let tok = token env tok in
      A.Call
        ( A.Id [ (A.builtin "exec", tok (* not really an exec token *)) ],
          Parse_info.fake_bracket tok
            [ (* TODO insert content of backquote expr *) ] )
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Throw_exp (v1, v2) ->
      let v1 = (* pattern [tT][hH][rR][oO][wW] *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
(* TODO A.Throw is a stmt, when it should be an expr *)

and map_property_element (env : env) ((v1, v2) : CST.property_element) =
  let v1 = map_variable_name env v1 in
  let v2 =
    match v2 with
    | Some x -> Some (map_property_initializer env x)
    | None -> None
  in
  (v1, v2)

and map_property_initializer (env : env) ((v1, v2) : CST.property_initializer) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_expression env v2 in
  v2

and map_scope_resolution_qualifier (env : env)
    (x : CST.scope_resolution_qualifier) =
  match x with
  | `Rela_scope x -> map_relative_scope env x
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      A.Id (map_name env tok)
  | `Rese_id x -> map_reserved_identifier env x
  | `Qual_name x -> A.Id (map_qualified_name env x)
  | `Dere_exp x -> map_dereferencable_expression env x

and map_scoped_property_access_expression (env : env)
    ((v1, v2, v3) : CST.scoped_property_access_expression) =
  let v1 = map_scope_resolution_qualifier env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 = map_variable_name_ env v3 in
  A.Call (A.Class_get (v1, v2, v3), Parse_info.fake_bracket v2 [])

and map_sequence_expression (env : env) ((v1, v2, v3) : CST.sequence_expression)
    =
  let v1 = map_expression env v1 in
  let v2 = (* "," *) token env v2 in
  let v3 =
    match v3 with
    | `Seq_exp x -> map_sequence_expression env x
    | `Exp x -> [ map_expression env x ]
  in
  v1 :: v3

and map_statement (env : env) (x : CST.statement) =
  match x with
  | `Empty_stmt tok -> (* ";" *) map_empty_block env (token env tok)
  | `Comp_stmt x -> map_compound_statement env x
  | `Named_label_stmt x -> map_named_label_statement env x
  | `Exp_stmt (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_semicolon env v2 in
      Expr (v1, v2)
  | `If_stmt (v1, v2, v3) ->
      let v1 = (* pattern [iI][fF] *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 =
        match v3 with
        | `Choice_empty_stmt_rep_else_if_clause_opt_else_clause (v1, v2, v3) ->
            let v1 = map_statement env v1 in
            let v2 = Common.map (map_else_if_clause env) v2 in
            let v3 =
              match v3 with
              | Some x -> map_else_clause env x
              | None -> map_empty_block env Parse_info.unsafe_sc
            in
            (v1, v2, v3)
        | `Colon_blk_rep_else_if_clause_2_opt_else_clause_2_pat_b10beb6_choice_auto_semi
            (v1, v2, v3, v4, v5) ->
            let v1 = map_colon_block env v1 in
            let v2 = Common.map (map_else_if_clause_2 env) v2 in
            let v3 =
              match v3 with
              | Some x -> map_else_clause_2 env x
              | None -> map_empty_block env Parse_info.unsafe_sc
            in
            let v4 = (* pattern [eE][nN][dD][iI][fF] *) token env v4 in
            let v5 = map_semicolon env v5 in
            (v1, v2, v3)
      in
      let stmt, elseifs, else_ = v3 in
      let else_ = chain_else_if env elseifs else_ in
      A.If (v1, v2, stmt, else_)
  | `Switch_stmt (v1, v2, v3) ->
      let v1 = (* pattern [sS][wW][iI][tT][cC][hH] *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_switch_block env v3 in
      A.Switch (v1, v2, v3)
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
            v1
      in
      A.While (v1, v2, v3)
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* pattern [dD][oO] *) token env v1 in
      let v2 = map_statement env v2 in
      let v3 = (* pattern [wW][hH][iI][lL][eE] *) token env v3 in
      let v4 = map_parenthesized_expression env v4 in
      let v5 = map_semicolon env v5 in
      A.Do (v1, v2, v4)
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = (* pattern [fF][oO][rR] *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> map_expressions env x
        | None -> []
      in
      let v4 = (* ";" *) token env v4 in
      let v5 =
        match v5 with
        | Some x -> map_expressions env x
        | None -> []
      in
      let v6 = (* ";" *) token env v6 in
      let v7 =
        match v7 with
        | Some x -> map_expressions env x
        | None -> []
      in
      let v8 = (* ")" *) token env v8 in
      let v9 =
        match v9 with
        | `Choice_auto_semi x -> map_empty_block env (map_semicolon env x)
        | `Choice_empty_stmt x -> map_statement env x
        | `COLON_rep_choice_empty_stmt_pat_1d5f5b3_choice_auto_semi
            (v1, v2, v3, v4) ->
            let v1 = (* ":" *) token env v1 in
            let v2 = Common.map (map_statement env) v2 in
            let v3 = (* pattern [eE][nN][dD][fF][oO][rR] *) token env v3 in
            let v4 = map_semicolon env v4 in
            A.Block (v1, v2, v3)
      in
      A.For (v1, v3, v5, v7, v9)
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
        | `Choice_auto_semi x -> map_empty_block env (map_semicolon env x)
        | `Choice_empty_stmt x -> map_statement env x
        | `Colon_blk_pat_25e0188_choice_auto_semi (v1, v2, v3) ->
            let v1 = map_colon_block env v1 in
            let v2 =
              (* pattern [eE][nN][dD][fF][oO][rR][eE][aA][cC][hH] *)
              token env v2
            in
            let v3 = map_semicolon env v3 in
            v1
      in
      A.Foreach (v1, v3, v4, v5, v7)
  | `Goto_stmt (v1, v2, v3) ->
      let v1 = (* pattern [gG][oO][tT][oO] *) token env v1 in
      let v2 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
        _str env v2
      in
      let v3 = map_semicolon env v3 in
      A.Goto (v1, v2)
  | `Cont_stmt (v1, v2, v3) ->
      let v1 = (* pattern [cC][oO][nN][tT][iI][nN][uU][eE] *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      let v3 = map_semicolon env v3 in
      A.Continue (v1, v2)
  | `Brk_stmt (v1, v2, v3) ->
      let v1 = (* pattern [bB][rR][eE][aA][kK] *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      let v3 = map_semicolon env v3 in
      A.Break (v1, v2)
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = (* pattern [rR][eE][tT][uU][rR][nN] *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      let v3 = map_semicolon env v3 in
      A.Return (v1, v2)
  | `Try_stmt (v1, v2, v3) ->
      let v1 = (* pattern [tT][rR][yY] *) token env v1 in
      let v2 = map_compound_statement env v2 in
      let catches, finallies = split_catch_finally env v3 [] [] in
      A.Try (v1, v2, catches, finallies)
  | `Decl_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "declare" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_declare_directive env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 =
        match v5 with
        | `Choice_empty_stmt x -> map_empty_statement_to_semicolon env x
        | `COLON_rep_choice_empty_stmt_pat_bb9603f_choice_auto_semi
            (v1, v2, v3, v4) ->
            let v1 = (* ":" *) token env v1 in
            let v2 = Common.map (map_statement env) v2 in
            let v3 =
              (* pattern [eE][nN][dD][dD][eE][cC][lL][aA][rR][eE] *)
              token env v3
            in
            let v4 = map_semicolon env v4 in
            v4
        | `Choice_auto_semi x -> map_semicolon env x
      in
      A.Expr (A.Call (A.Id [ (A.builtin "declare", v1) ], (v2, [ v3 ], v4)), v5)
  | `Echo_stmt (v1, v2, v3) ->
      let v1 = (* pattern [eE][cC][hH][oO] *) _str env v1 in
      let v2 = map_expressions env v2 in
      let v3 = map_semicolon env v3 in
      A.Expr (fake_call_to_builtin env v1 v2, v3)
  | `Unset_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "unset" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_variable env v3 in
      let v4 =
        Common.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_variable env v2 in
            v2)
          v4
      in
      let v5 = (* ")" *) token env v5 in
      let v6 = map_semicolon env v6 in
      A.Expr (A.Call (A.Id [ (A.builtin "unset", v1) ], (v2, v3 :: v4, v5)), v6)
  | `Const_decl x ->
      let consts = map_const_declaration_ env x in
      let consts = Common.map (fun c -> A.ConstantDef c) consts in
      stmt1 consts
  | `Func_defi (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> []
      in
      let v2 = map_function_definition_header env v2 in
      let v3 = map_compound_statement env v3 in
      let tok, is_ref, name, params, return = v2 in
      A.FuncDef
        {
          A.f_name = name;
          A.f_kind = (Function, tok);
          A.f_params = params;
          A.f_return_type = return;
          A.f_ref = is_ref;
          A.m_modifiers = [];
          A.f_attrs = v1;
          A.l_uses = [];
          A.f_body = v3;
        }
  | `Class_decl (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> []
      in
      let v2 =
        match v2 with
        | Some x -> (
            match x with
            | `Final_modi tok ->
                (* pattern [fF][iI][nN][aA][lL] *) [ (A.Final, token env tok) ]
            | `Abst_modi tok ->
                (* pattern [aA][bB][sS][tT][rR][aA][cC][tT] *)
                [ (A.Abstract, token env tok) ])
        | None -> []
      in
      let v3 = (* pattern [cC][lL][aA][sS][sS] *) token env v3 in
      let v4 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
        _str env v4
      in
      let v5 =
        match v5 with
        | Some x -> Some (List.hd (map_base_clause env x))
        | None -> None
      in
      let v6 =
        match v6 with
        | Some x -> map_class_interface_clause env x
        | None -> []
      in
      let v7 = map_declaration_list env v7 in
      let v8 =
        match v8 with
        | Some x -> Some (map_semicolon env x)
        | None -> None
      in
      let opn, decls, cls = v7 in
      let consts, vars, methods, uses = split_classmembers env decls in
      ClassDef
        {
          c_name = v4;
          c_kind = (A.Class, v3);
          c_extends = v5;
          c_implements = v6;
          c_uses = uses;
          c_enum_type = None;
          c_modifiers = v2;
          c_attrs = v1;
          c_constants = consts;
          c_variables = vars;
          c_methods = methods;
          c_braces = (opn, (), cls);
        }
  | `Inte_decl (v1, v2, v3, v4) ->
      let v1 =
        (* pattern [iI][nN][tT][eE][rR][fF][aA][cC][eE] *) token env v1
      in
      let v2 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
        _str env v2
      in
      let v3 =
        match v3 with
        (* TODO interfaces can extend multiple other interfaces, but we throw away everything but the first base interface here. *)
        | Some x -> Some (List.hd (map_base_clause env x))
        | None -> None
      in
      let v4 = map_declaration_list env v4 in
      let opn, decls, cls = v4 in
      let consts, vars, methods, uses = split_classmembers env decls in
      ClassDef
        {
          c_name = v2;
          c_kind = (A.Interface, v1);
          c_extends = v3;
          c_implements = [];
          c_uses = uses;
          c_enum_type = None;
          c_modifiers = [];
          c_attrs = [];
          c_constants = consts;
          c_variables = vars;
          c_methods = methods;
          c_braces = (opn, (), cls);
        }
  | `Trait_decl (v1, v2, v3) ->
      let v1 = (* pattern [tT][rR][aA][iI][tT] *) token env v1 in
      let v2 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
        _str env v2
      in
      let v3 = map_declaration_list env v3 in
      let opn, decls, cls = v3 in
      let consts, vars, methods, uses = split_classmembers env decls in
      ClassDef
        {
          c_name = v2;
          c_kind = (A.Trait, v1);
          c_extends = None;
          c_implements = [];
          c_uses = uses;
          c_enum_type = None;
          c_modifiers = [];
          c_attrs = [];
          c_constants = consts;
          c_variables = vars;
          c_methods = methods;
          c_braces = (opn, (), cls);
        }
  | `Enum_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> []
      in
      let v2 = (* pattern [eE][nN][uU][mM] *) token env v2 in
      let v3 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
        _str env v3
      in
      let v4 =
        match v4 with
        | Some x ->
            Some { A.e_base = map_return_type env x; A.e_constraint = None }
        | None -> None
      in
      let v5 =
        match v5 with
        | Some x -> map_class_interface_clause env x
        | None -> []
      in
      let opn, decls, cls = map_enum_declaration_list env v6 in
      let consts, vars, methods, uses = split_classmembers env decls in
      ClassDef
        {
          c_name = v3;
          c_kind = (A.Enum, v2);
          c_extends = None;
          c_implements = v5;
          c_uses = uses;
          c_enum_type = v4;
          c_modifiers = [];
          c_attrs = v1;
          c_constants = consts;
          c_variables = vars;
          c_methods = methods;
          c_braces = (opn, (), cls);
        }
  | `Name_defi (v1, v2) ->
      let v1 =
        (* pattern [nN][aA][mM][eE][sS][pP][aA][cC][eE] *) token env v1
      in
      let v2 =
        match v2 with
        | `Name_name_choice_auto_semi (v1, v2) ->
            let v1 = map_namespace_name env v1 in
            let v2 = map_semicolon env v2 in
            (v1, Parse_info.fake_bracket v2 [])
        | `Opt_name_name_comp_stmt (v1, v2) ->
            let v1 =
              match v1 with
              | Some x -> map_namespace_name env x
              | None -> []
            in
            let v2 = map_compound_statement_ env v2 in
            (v1, v2)
      in
      let name, block = v2 in
      A.NamespaceDef (v1, name, block)
  | `Name_use_decl (v1, v2, v3, v4) ->
      let use_tok = (* pattern [uU][sS][eE] *) token env v1 in
      let v2_todo =
        match v2 with
        | Some x -> Some (map_anon_choice_pat_174c3a5_81b85de env x)
        | None -> None
      in
      let v3 =
        match v3 with
        | `Name_use_clause_rep_COMMA_name_use_clause (v1, v2) ->
            let v1 = map_namespace_use_clause env v1 in
            let v2 =
              Common.map
                (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = map_namespace_use_clause env v2 in
                  v2)
                v2
            in
            let names = v1 :: v2 in
            Common.map
              (fun (name, alias) -> A.NamespaceUse (use_tok, name, alias))
              names
        | `Opt_BSLASH_name_name_BSLASH_name_use_group (v1, v2, v3, v4) ->
            let v1 =
              match v1 with
              | Some tok -> (* "\\" *) Some (token env tok)
              | None -> None
            in
            let v2 = map_namespace_name env v2 in
            let v3 = (* "\\" *) token env v3 in
            let v4 = map_namespace_use_group env v4 in
            Common.map
              (fun (name, alias) -> A.NamespaceUse (use_tok, v2 @ name, alias))
              v4
      in
      let v4 = map_semicolon env v4 in
      stmt1 v3
  | `Global_decl (v1, v2, v3, v4) ->
      let v1 = (* pattern [gG][lL][oO][bB][aA][lL] *) token env v1 in
      let v2 = map_variable_name_ env v2 in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_variable_name_ env v2 in
            v2)
          v3
      in
      let v4 = map_semicolon env v4 in
      Global (v1, v2 :: v3)
  | `Func_static_decl (v1, v2, v3, v4) ->
      let v1 = (* pattern [sS][tT][aA][tT][iI][cC] *) token env v1 in
      let v2 = map_static_variable_declaration env v2 in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_static_variable_declaration env v2 in
            v2)
          v3
      in
      let v4 = map_semicolon env v4 in
      StaticVars (v1, v2 :: v3)

and map_static_variable_declaration (env : env)
    ((v1, v2) : CST.static_variable_declaration) : A.var * A.expr option =
  let v1 = map_variable_name env v1 in
  let v2 =
    match v2 with
    | Some x -> Some (map_property_initializer env x)
    | None -> None
  in
  (v1, v2)

and map_subscript_expression (env : env) ((v1, v2) : CST.subscript_expression) =
  let v1 = map_dereferencable_expression env v1 in
  let v2 =
    match v2 with
    | `LBRACK_opt_exp_RBRACK (v1, v2, v3) ->
        let v1 = (* "[" *) token env v1 in
        let v2 =
          match v2 with
          | Some x -> Some (map_expression env x)
          | None -> None
        in
        let v3 = (* "]" *) token env v3 in
        (v1, v2, v3)
    | `LCURL_exp_RCURL (v1, v2, v3) ->
        let v1 = (* "{" *) token env v1 in
        let v2 = Some (map_expression env v2) in
        let v3 = (* "}" *) token env v3 in
        (v1, v2, v3)
  in
  A.Array_get (v1, v2)

and map_switch_block (env : env) (x : CST.switch_block) =
  match x with
  | `LCURL_rep_choice_case_stmt_RCURL (v1, v2, v3) ->
      let v1 = (* "{" *) token env v1 in
      let v2 = Common.map (map_anon_choice_case_stmt_f1b35bc env) v2 in
      let v3 = (* "}" *) token env v3 in
      v2
  | `COLON_rep_choice_case_stmt_pat_0b47e00_choice_auto_semi (v1, v2, v3, v4) ->
      let v1 = (* ":" *) token env v1 in
      let v2 = Common.map (map_anon_choice_case_stmt_f1b35bc env) v2 in
      let v3 =
        (* pattern [eE][nN][dD][sS][wW][iI][tT][cC][hH] *) token env v3
      in
      let v4 = map_semicolon env v4 in
      v2

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
      A.Cast (v2, v4)

and map_unary_op_expression (env : env) (x : CST.unary_op_expression) =
  match x with
  | `AT_exp (v1, v2) ->
      let v1 = (* "@" *) token env v1 in
      let v2 = map_expression env v2 in
      v2 (* TODO include error control operator "@" *)
  | `Choice_PLUS_exp (v1, v2) ->
      let v1 =
        match v1 with
        | `PLUS tok -> (* "+" *) (G_.Plus, token env tok)
        | `DASH tok -> (* "-" *) (G_.Minus, token env tok)
        | `TILDE tok -> (* "~" *) (G_.BitNot, token env tok)
        | `BANG tok -> (* "!" *) (G_.Not, token env tok)
      in
      let v2 = map_expression env v2 in
      A.Unop (v1, v2)

and map_update_expression (env : env) (x : CST.update_expression) =
  match x with
  | `Choice_cast_var_PLUSPLUS (v1, v2) ->
      let v1 = map_variable env v1 in
      let v2 = (* "++" *) token env v2 in
      A.Postfix ((G_.Incr, v2), v1)
  | `Choice_cast_var_DASHDASH (v1, v2) ->
      let v1 = map_variable env v1 in
      let v2 = (* "--" *) token env v2 in
      A.Postfix ((G_.Decr, v2), v1)
  | `PLUSPLUS_choice_cast_var (v1, v2) ->
      let v1 = (* "++" *) token env v1 in
      let v2 = map_variable env v2 in
      A.Infix ((G_.Incr, v1), v2)
  | `DASHDASH_choice_cast_var (v1, v2) ->
      let v1 = (* "--" *) token env v1 in
      let v2 = map_variable env v2 in
      A.Infix ((G_.Decr, v1), v2)

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

and map_use_declaration (env : env) ((v1, v2, v3, v4) : CST.use_declaration) :
    classmember list =
  let v1 = (* pattern [uU][sS][eE] *) token env v1 in
  let v2 = A.Hint (map_anon_choice_name_062e4f2 env v2) in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = A.Hint (map_anon_choice_name_062e4f2 env v2) in
        v2)
      v3
  in
  let v4 =
    match v4 with
    | `Use_list x -> map_use_list env x
    | `Choice_auto_semi x -> map_semicolon env x
  in
  let uses = v2 :: v3 in
  Common.map (fun u -> UseTrait u) uses

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
    Common.map
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
      A.Cast (v2, v4)
  | `Choice_choice_dyna_var_name x -> map_callable_variable env x
  | `Scoped_prop_access_exp x -> map_scoped_property_access_expression env x
  | `Member_access_exp x -> map_member_access_expression env x
  | `Null_member_access_exp x -> map_nullsafe_member_access_expression env x

and map_variable_name_ (env : env) (x : CST.variable_name_) : A.expr =
  match x with
  | `Dyna_var_name x -> map_dynamic_variable_name env x
  | `Var_name x -> A.Id [ map_variable_name env x ]

and map_variadic_unpacking (env : env) ((v1, v2) : CST.variadic_unpacking) =
  let v1 = (* "..." *) token env v1 in
  let v2 = map_expression env v2 in
  A.Unpack v2

let map_program (env : env) ((v1, v2) : CST.program) : A.program =
  let v1 =
    match v1 with
    | Some x -> Some (map_text env x)
    | None -> None
  in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = (* pattern <\?([pP][hH][pP]|=)? *) token env v1 in
        let v2 = Common.map (map_statement env) v2 in
        v2
    | None -> []
  in
  v2

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
