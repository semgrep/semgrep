(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
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

module CST = Tree_sitter_python.CST
module PI = Parse_info
module H = Parse_tree_sitter_helpers
open AST_python

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Python parser using tree-sitter-lang/semgrep-python and converting
 * to pfff/lang_python/parsing/AST_Python.ml
 *
 * The resulting AST can then be converted to the generic AST by using
 * Python_to_generic.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type env = unit H.env

let token = H.token

let str = H.str

(* this is not used anyway by Python_to_generic.ml, so I took whatever *)
let no_ctx = Param

let _fake = PI.fake_info

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-python/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the python CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

let todo (env : env) _ = failwith "not implemented"

let map_keyword_identifier (env : env) (x : CST.keyword_identifier) : name =
  match x with
  | `Print tok -> (* "print" *) str env tok
  | `Exec tok -> (* "exec" *) str env tok
  | `Async tok -> (* "async" *) str env tok
  | `Await tok -> (* "await" *) str env tok

let map_escape_interpolation (env : env) (x : CST.escape_interpolation) =
  match x with
  | `LCURLLCURL tok -> (* "{{" *) str env tok
  | `RCURLRCURL tok -> (* "}}" *) str env tok

let map_import_prefix (env : env) (xs : CST.import_prefix) : tok list =
  List.map (token env (* "." *)) xs

let map_dotted_name (env : env) ((v1, v2) : CST.dotted_name) : dotted_name =
  let v1 = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = (* "." *) token env v1 in
        let v2 =
          (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v2
        in
        v2)
      v2
  in
  v1 :: v2

let rec map_anon_choice_id_b80cb38 (env : env) (x : CST.anon_choice_id_b80cb38)
    : expr =
  match x with
  | `Id tok ->
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok in
      todo env id
  | `Choice_print x ->
      let id = map_keyword_identifier env x in
      todo env id
  | `Subs x -> map_subscript env x
  | `Attr x -> map_attribute env x

and map_anon_choice_pair_002ffed (env : env) (x : CST.anon_choice_pair_002ffed)
    =
  match x with
  | `Pair x -> map_pair env x
  | `Dict_splat x -> map_dictionary_splat env x

and map_anon_choice_type_03d361f (env : env) (x : CST.anon_choice_type_03d361f)
    =
  match x with
  | `Exp x -> map_type_ env x
  | `Yield x -> map_yield env x
  | `List_splat x -> map_list_splat env x
  | `Paren_list_splat x -> map_parenthesized_list_splat env x

and map_anon_choice_type_a577897 (env : env) (x : CST.anon_choice_type_a577897)
    =
  match x with
  | `Exp x -> map_type_ env x
  | `Slice (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | Some x -> map_type_ env x
        | None -> todo env ()
      in
      let v2 = (* ":" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> map_type_ env x
        | None -> todo env ()
      in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            let v1 = (* ":" *) token env v1 in
            let v2 =
              match v2 with
              | Some x -> map_type_ env x
              | None -> todo env ()
            in
            todo env (v1, v2)
        | None -> todo env ()
      in
      todo env (v1, v2, v3, v4)

and map_anon_choice_type_aad5b2d (env : env) (x : CST.anon_choice_type_aad5b2d)
    =
  match x with
  | `Exp x -> map_type_ env x
  | `List_splat x -> map_list_splat env x
  | `Dict_splat x -> map_dictionary_splat env x
  | `Paren_list_splat x -> map_parenthesized_list_splat env x
  | `Kw_arg (v1, v2, v3) ->
      let id =
        match v1 with
        | `Id tok ->
            (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok
        | `Choice_print x -> map_keyword_identifier env x
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_type_ env v3 in
      todo env (id, v2, v3)

and map_argument_list (env : env) ((v1, v2, v3, v4) : CST.argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_type_aad5b2d env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let v1 = (* "," *) token env v1 in
              let v2 = map_anon_choice_type_aad5b2d env v2 in
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

and map_attribute (env : env) ((v1, v2, v3) : CST.attribute) : expr =
  let e = map_primary_expression env v1 in
  let tdot = (* "." *) token env v2 in
  let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v3 in
  Attribute (e, tdot, id, no_ctx)

and map_binary_operator (env : env) (x : CST.binary_operator) : expr =
  match x with
  | `Prim_exp_PLUS_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      todo env (v1, v2, v3)
  | `Prim_exp_DASH_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      todo env (v1, v2, v3)
  | `Prim_exp_STAR_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      todo env (v1, v2, v3)
  | `Prim_exp_AT_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "@" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      todo env (v1, v2, v3)
  | `Prim_exp_SLASH_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      todo env (v1, v2, v3)
  | `Prim_exp_PERC_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      todo env (v1, v2, v3)
  | `Prim_exp_SLASHSLASH_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "//" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      todo env (v1, v2, v3)
  | `Prim_exp_STARSTAR_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "**" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      todo env (v1, v2, v3)
  | `Prim_exp_BAR_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      todo env (v1, v2, v3)
  | `Prim_exp_AMP_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      todo env (v1, v2, v3)
  | `Prim_exp_HAT_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      todo env (v1, v2, v3)
  | `Prim_exp_LTLT_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      todo env (v1, v2, v3)
  | `Prim_exp_GTGT_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      todo env (v1, v2, v3)

and map_boolean_operator (env : env) (x : CST.boolean_operator) =
  match x with
  | `Exp_and_exp (v1, v2, v3) ->
      let v1 = map_type_ env v1 in
      let v2 = (* "and" *) token env v2 in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `Exp_or_exp (v1, v2, v3) ->
      let v1 = map_type_ env v1 in
      let v2 = (* "or" *) token env v2 in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)

and map_collection_elements (env : env) ((v1, v2, v3) : CST.collection_elements)
    =
  let v1 = map_anon_choice_type_03d361f env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_type_03d361f env v2 in
        todo env (v1, v2))
      v2
  in
  let v3 =
    match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_comprehension_clauses (env : env) ((v1, v2) : CST.comprehension_clauses)
    =
  let v1 = map_for_in_clause env v1 in
  let v2 =
    List.map
      (fun x ->
        match x with
        | `For_in_clause x -> map_for_in_clause env x
        | `If_clause x -> map_if_clause env x)
      v2
  in
  todo env (v1, v2)

and map_dictionary_splat (env : env) ((v1, v2) : CST.dictionary_splat) =
  let v1 = (* "**" *) token env v1 in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

and map_dictionary_splat_pattern (env : env)
    ((v1, v2) : CST.dictionary_splat_pattern) =
  let v1 = (* "**" *) token env v1 in
  let v2 = map_anon_choice_id_b80cb38 env v2 in
  todo env (v1, v2)

and map_expression (env : env) (x : CST.expression) : expr =
  match x with
  | `Comp_op (v1, v2) ->
      let v1 = map_primary_expression env v1 in
      let v2 =
        List.map
          (fun (v1, v2) ->
            let v1 =
              match v1 with
              | `LT tok -> (* "<" *) token env tok
              | `LTEQ tok -> (* "<=" *) token env tok
              | `EQEQ tok -> (* "==" *) token env tok
              | `BANGEQ tok -> (* "!=" *) token env tok
              | `GTEQ tok -> (* ">=" *) token env tok
              | `GT tok -> (* ">" *) token env tok
              | `LTGT tok -> (* "<>" *) token env tok
              | `In tok -> (* "in" *) token env tok
              | `Not_in (v1, v2) ->
                  let v1 = (* "not" *) token env v1 in
                  let v2 = (* "in" *) token env v2 in
                  todo env (v1, v2)
              | `Is tok -> (* "is" *) token env tok
              | `Is_not (v1, v2) ->
                  let v1 = (* "is" *) token env v1 in
                  let v2 = (* "not" *) token env v2 in
                  todo env (v1, v2)
            in
            let v2 = map_primary_expression env v2 in
            todo env (v1, v2))
          v2
      in
      todo env (v1, v2)
  | `Not_op (v1, v2) ->
      let v1 = (* "not" *) token env v1 in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
  | `Bool_op x -> map_boolean_operator env x
  | `Await (v1, v2) ->
      let v1 = (* "await" *) token env v1 in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
  | `Lambda (v1, v2, v3, v4) ->
      let v1 = (* "lambda" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_lambda_parameters env x
        | None -> todo env ()
      in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_type_ env v4 in
      todo env (v1, v2, v3, v4)
  | `Prim_exp x -> map_primary_expression env x
  | `Cond_exp (v1, v2, v3, v4, v5) ->
      let v1 = map_type_ env v1 in
      let v2 = (* "if" *) token env v2 in
      let v3 = map_type_ env v3 in
      let v4 = (* "else" *) token env v4 in
      let v5 = map_type_ env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Named_exp (v1, v2, v3) ->
      let v1 =
        (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) token env v1
      in
      let v2 = (* ":=" *) token env v2 in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)

and map_expression_list (env : env) ((v1, v2) : CST.expression_list) =
  let v1 = map_type_ env v1 in
  let v2 =
    match v2 with
    | `COMMA tok -> (* "," *) token env tok
    | `Rep1_COMMA_exp_opt_COMMA (v1, v2) ->
        let v1 =
          List.map
            (fun (v1, v2) ->
              let v1 = (* "," *) token env v1 in
              let v2 = map_type_ env v2 in
              todo env (v1, v2))
            v1
        in
        let v2 =
          match v2 with
          | Some tok -> (* "," *) token env tok
          | None -> todo env ()
        in
        todo env (v1, v2)
  in
  todo env (v1, v2)

and map_expression_within_for_in_clause (env : env)
    (x : CST.expression_within_for_in_clause) =
  match x with
  | `Exp x -> map_type_ env x
  | `Lambda_within_for_in_clause (v1, v2, v3, v4) ->
      let v1 = (* "lambda" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_lambda_parameters env x
        | None -> todo env ()
      in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_expression_within_for_in_clause env v4 in
      todo env (v1, v2, v3, v4)

and map_expressions (env : env) (x : CST.expressions) =
  match x with
  | `Exp x -> map_type_ env x
  | `Exp_list x -> map_expression_list env x

and map_for_in_clause (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.for_in_clause) =
  let v1 =
    match v1 with
    | Some tok -> (* "async" *) token env tok
    | None -> todo env ()
  in
  let v2 = (* "for" *) token env v2 in
  let v3 = map_left_hand_side env v3 in
  let v4 = (* "in" *) token env v4 in
  let v5 = map_expression_within_for_in_clause env v5 in
  let v6 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_expression_within_for_in_clause env v2 in
        todo env (v1, v2))
      v6
  in
  let v7 =
    match v7 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ()
  in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and map_format_expression (env : env) ((v1, v2, v3) : CST.format_expression) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_format_specifier (env : env) ((v1, v2) : CST.format_specifier) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    List.map
      (fun x ->
        match x with
        | `LBRA tok -> (* [^{}\n]+ *) token env tok
        | `Format_exp x -> map_format_expression env x)
      v2
  in
  todo env (v1, v2)

and map_generator_expression (env : env)
    ((v1, v2, v3, v4) : CST.generator_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 = map_comprehension_clauses env v3 in
  let v4 = (* ")" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_if_clause (env : env) ((v1, v2) : CST.if_clause) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

and map_interpolation (env : env) ((v1, v2, v3, v4, v5) : CST.interpolation) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    match v3 with
    | Some tok -> (* pattern ![a-z] *) token env tok
    | None -> todo env ()
  in
  let v4 =
    match v4 with
    | Some x -> map_format_specifier env x
    | None -> todo env ()
  in
  let v5 = (* "}" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_lambda_parameters (env : env) (x : CST.lambda_parameters) =
  map_parameters_ env x

and map_left_hand_side (env : env) (x : CST.left_hand_side) =
  match x with
  | `Pat x -> map_pattern env x
  | `Pat_list (v1, v2) ->
      let v1 = map_pattern env v1 in
      let v2 =
        match v2 with
        | `COMMA tok -> (* "," *) token env tok
        | `Rep1_COMMA_pat_opt_COMMA (v1, v2) ->
            let v1 =
              List.map
                (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = map_pattern env v2 in
                  todo env (v1, v2))
                v1
            in
            let v2 =
              match v2 with
              | Some tok -> (* "," *) token env tok
              | None -> todo env ()
            in
            todo env (v1, v2)
      in
      todo env (v1, v2)

and map_list_splat (env : env) ((v1, v2) : CST.list_splat) =
  let v1 = (* "*" *) token env v1 in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

and map_list_splat_pattern (env : env) ((v1, v2) : CST.list_splat_pattern) :
    pattern =
  let v1 = (* "*" *) token env v1 in
  let v2 = map_anon_choice_id_b80cb38 env v2 in
  todo env (v1, v2)

and map_pair (env : env) ((v1, v2, v3) : CST.pair) =
  let v1 = map_type_ env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_type_ env v3 in
  todo env (v1, v2, v3)

and map_parameter (env : env) (x : CST.parameter) : parameter =
  match x with
  | `Id tok ->
      let id =
        (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) token env tok
      in
      todo env id
  | `Typed_param (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Id tok ->
            let id =
              (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok
            in
            todo env id
        | `List_splat_pat x -> map_list_splat_pattern env x
        | `Dict_splat_pat x -> map_dictionary_splat_pattern env x
      in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `Defa_param (v1, v2, v3) ->
      let v1 =
        (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) token env v1
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `Typed_defa_param (v1, v2, v3, v4, v5) ->
      let v1 =
        (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) token env v1
      in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_type_ env v3 in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_type_ env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `List_splat_pat x ->
      let pat = map_list_splat_pattern env x in
      todo env pat
  | `Tuple_pat x ->
      let pat = map_tuple_pattern env x in
      todo env pat
  | `STAR tok ->
      let t = (* "*" *) token env tok in
      todo env t
  | `Dict_splat_pat x ->
      let pat = map_dictionary_splat_pattern env x in
      todo env pat

and map_parameters_ (env : env) ((v1, v2, v3) : CST.parameters_) =
  let v1 = map_parameter env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_parameter env v2 in
        todo env (v1, v2))
      v2
  in
  let v3 =
    match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_parenthesized_list_splat (env : env)
    ((v1, v2, v3) : CST.parenthesized_list_splat) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | `Paren_list_splat x -> map_parenthesized_list_splat env x
    | `List_splat x -> map_list_splat env x
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_pattern (env : env) (x : CST.pattern) : pattern =
  match x with
  | `Id tok ->
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok in
      todo env id
  | `Choice_print x ->
      let id = map_keyword_identifier env x in
      todo env id
  | `Subs x -> map_subscript env x
  | `Attr x -> map_attribute env x
  | `List_splat_pat x -> map_list_splat_pattern env x
  | `Tuple_pat x -> map_tuple_pattern env x
  | `List_pat (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_patterns env x
        | None -> todo env ()
      in
      let v3 = (* "]" *) token env v3 in
      todo env (v1, v2, v3)

and map_patterns (env : env) ((v1, v2, v3) : CST.patterns) =
  let v1 = map_pattern env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_pattern env v2 in
        todo env (v1, v2))
      v2
  in
  let v3 =
    match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_primary_expression (env : env) (x : CST.primary_expression) : expr =
  match x with
  | `Bin_op x -> map_binary_operator env x
  | `Id tok ->
      let id =
        (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) token env tok
      in
      todo env id
  | `Choice_print x ->
      let id = map_keyword_identifier env x in
      todo env id
  | `Str x ->
      let x = map_string_ env x in
      todo env x
  | `Conc_str (v1, v2) ->
      let v1 = map_string_ env v1 in
      let v2 = List.map (map_string_ env) v2 in
      todo env (v1, v2)
  | `Int tok ->
      let s = (* integer *) str env tok in
      todo env s
  | `Float tok ->
      let s = (* float *) str env tok in
      todo env s
  | `True tok ->
      let t = (* "True" *) token env tok in
      Bool (true, t)
  | `False tok ->
      let t = (* "False" *) token env tok in
      Bool (false, t)
  | `None tok ->
      let t = (* "None" *) token env tok in
      None_ t
  | `Un_op (v1, v2) ->
      let v1 =
        match v1 with
        | `PLUS tok -> (* "+" *) token env tok
        | `DASH tok -> (* "-" *) token env tok
        | `TILDE tok -> (* "~" *) token env tok
      in
      let v2 = map_primary_expression env v2 in
      todo env (v1, v2)
  | `Attr x -> map_attribute env x
  | `Subs x -> map_subscript env x
  | `Call (v1, v2) ->
      let v1 = map_primary_expression env v1 in
      let v2 =
        match v2 with
        | `Gene_exp x -> map_generator_expression env x
        | `Arg_list x -> map_argument_list env x
      in
      todo env (v1, v2)
  | `List (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_collection_elements env x
        | None -> todo env ()
      in
      let v3 = (* "]" *) token env v3 in
      todo env (v1, v2, v3)
  | `List_comp (v1, v2, v3, v4) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = map_type_ env v2 in
      let v3 = map_comprehension_clauses env v3 in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Dict (v1, v2, v3, v4) ->
      let v1 = (* "{" *) token env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let v1 = map_anon_choice_pair_002ffed env v1 in
            let v2 =
              List.map
                (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = map_anon_choice_pair_002ffed env v2 in
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
      let v4 = (* "}" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Dict_comp (v1, v2, v3, v4) ->
      let v1 = (* "{" *) token env v1 in
      let v2 = map_pair env v2 in
      let v3 = map_comprehension_clauses env v3 in
      let v4 = (* "}" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Set (v1, v2, v3) ->
      let v1 = (* "{" *) token env v1 in
      let v2 = map_collection_elements env v2 in
      let v3 = (* "}" *) token env v3 in
      todo env (v1, v2, v3)
  | `Set_comp (v1, v2, v3, v4) ->
      let v1 = (* "{" *) token env v1 in
      let v2 = map_type_ env v2 in
      let v3 = map_comprehension_clauses env v3 in
      let v4 = (* "}" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Tuple (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_collection_elements env x
        | None -> todo env ()
      in
      let v3 = (* ")" *) token env v3 in
      todo env (v1, v2, v3)
  | `Paren_exp (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 =
        match v2 with
        | `Exp x -> map_type_ env x
        | `Yield x -> map_yield env x
      in
      let v3 = (* ")" *) token env v3 in
      todo env (v1, v2, v3)
  | `Gene_exp x -> map_generator_expression env x
  | `Ellips tok ->
      let t = (* "..." *) token env tok in
      Ellipsis t

and map_string_ (env : env) ((v1, v2, v3) : CST.string_) =
  let v1 = (* string_start *) token env v1 in
  let v2 =
    List.map
      (fun x ->
        match x with
        | `Interp x -> map_interpolation env x
        | `Esc_interp x -> map_escape_interpolation env x
        | `Esc_seq tok -> (* escape_sequence *) str env tok
        | `Not_esc_seq tok -> (* "\\" *) str env tok
        | `Str_content tok -> (* string_content *) str env tok)
      v2
  in
  let v3 = (* string_end *) token env v3 in
  todo env (v1, v2, v3)

and map_subscript (env : env) ((v1, v2, v3, v4, v5, v6) : CST.subscript) =
  let v1 = map_primary_expression env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = map_anon_choice_type_a577897 env v3 in
  let v4 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_type_a577897 env v2 in
        todo env (v1, v2))
      v4
  in
  let v5 =
    match v5 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ()
  in
  let v6 = (* "]" *) token env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and map_tuple_pattern (env : env) ((v1, v2, v3) : CST.tuple_pattern) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_patterns env x
    | None -> todo env ()
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_type_ (env : env) (x : CST.type_) : type_ = map_expression env x

and map_yield (env : env) ((v1, v2) : CST.yield) =
  let v1 = (* "yield" *) token env v1 in
  let v2 =
    match v2 with
    | `From_exp (v1, v2) ->
        let v1 = (* "from" *) token env v1 in
        let v2 = map_type_ env v2 in
        todo env (v1, v2)
    | `Opt_choice_exp opt -> (
        match opt with
        | Some x -> map_expressions env x
        | None -> todo env ())
  in
  todo env (v1, v2)

let map_relative_import (env : env) ((v1, v2) : CST.relative_import) =
  let v1 = map_import_prefix env v1 in
  let v2 =
    match v2 with
    | Some x -> map_dotted_name env x
    | None -> todo env ()
  in
  todo env (v1, v2)

let map_with_item (env : env) ((v1, v2) : CST.with_item) =
  let v1 = map_type_ env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = (* "as" *) token env v1 in
        let v2 = map_pattern env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  todo env (v1, v2)

let rec map_assignment (env : env) ((v1, v2) : CST.assignment) =
  let v1 = map_left_hand_side env v1 in
  let v2 =
    match v2 with
    | `EQ_right_hand_side (v1, v2) ->
        let v1 = (* "=" *) token env v1 in
        let v2 = map_right_hand_side env v2 in
        todo env (v1, v2)
    | `COLON_type (v1, v2) ->
        let v1 = (* ":" *) token env v1 in
        let v2 = map_type_ env v2 in
        todo env (v1, v2)
    | `COLON_type_EQ_right_hand_side (v1, v2, v3, v4) ->
        let v1 = (* ":" *) token env v1 in
        let v2 = map_type_ env v2 in
        let v3 = (* "=" *) token env v3 in
        let v4 = map_right_hand_side env v4 in
        todo env (v1, v2, v3, v4)
  in
  todo env (v1, v2)

and map_augmented_assignment (env : env)
    ((v1, v2, v3) : CST.augmented_assignment) =
  let v1 = map_left_hand_side env v1 in
  let v2 =
    match v2 with
    | `PLUSEQ tok -> (* "+=" *) token env tok
    | `DASHEQ tok -> (* "-=" *) token env tok
    | `STAREQ tok -> (* "*=" *) token env tok
    | `SLASHEQ tok -> (* "/=" *) token env tok
    | `ATEQ tok -> (* "@=" *) token env tok
    | `SLASHSLASHEQ tok -> (* "//=" *) token env tok
    | `PERCEQ tok -> (* "%=" *) token env tok
    | `STARSTAREQ tok -> (* "**=" *) token env tok
    | `GTGTEQ tok -> (* ">>=" *) token env tok
    | `LTLTEQ tok -> (* "<<=" *) token env tok
    | `AMPEQ tok -> (* "&=" *) token env tok
    | `HATEQ tok -> (* "^=" *) token env tok
    | `BAREQ tok -> (* "|=" *) token env tok
  in
  let v3 = map_right_hand_side env v3 in
  todo env (v1, v2, v3)

and map_right_hand_side (env : env) (x : CST.right_hand_side) =
  match x with
  | `Exp x -> map_type_ env x
  | `Exp_list x -> map_expression_list env x
  | `Assign x -> map_assignment env x
  | `Augm_assign x -> map_augmented_assignment env x
  | `Yield x -> map_yield env x

let map_decorator (env : env) ((v1, v2, v3) : CST.decorator) =
  let v1 = (* "@" *) token env v1 in
  let v2 = map_primary_expression env v2 in
  let v3 = (* newline *) token env v3 in
  todo env (v1, v2, v3)

let map_chevron (env : env) ((v1, v2) : CST.chevron) =
  let v1 = (* ">>" *) token env v1 in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

let map_parameters (env : env) ((v1, v2, v3) : CST.parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_lambda_parameters env x
    | None -> todo env ()
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

let map_anon_choice_dotted_name_c5c573a (env : env)
    (x : CST.anon_choice_dotted_name_c5c573a) =
  match x with
  | `Dotted_name x -> map_dotted_name env x
  | `Alia_import (v1, v2, v3) ->
      let v1 = map_dotted_name env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 =
        (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) token env v3
      in
      todo env (v1, v2, v3)

let map_with_clause (env : env) (x : CST.with_clause) =
  match x with
  | `With_item_rep_COMMA_with_item (v1, v2) ->
      let v1 = map_with_item env v1 in
      let v2 =
        List.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_with_item env v2 in
            todo env (v1, v2))
          v2
      in
      todo env (v1, v2)
  | `LPAR_with_item_rep_COMMA_with_item_RPAR (v1, v2, v3, v4) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_with_item env v2 in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_with_item env v2 in
            todo env (v1, v2))
          v3
      in
      let v4 = (* ")" *) token env v4 in
      todo env (v1, v2, v3, v4)

let map_expression_statement (env : env) (x : CST.expression_statement) : expr =
  match x with
  | `Exp x -> map_type_ env x
  | `Exp_rep_COMMA_exp_opt_COMMA (v1, v2, v3) ->
      let v1 = map_type_ env v1 in
      let v2 =
        List.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_type_ env v2 in
            todo env (v1, v2))
          v2
      in
      let v3 =
        match v3 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ()
      in
      todo env (v1, v2, v3)
  | `Assign x -> map_assignment env x
  | `Augm_assign x -> map_augmented_assignment env x
  | `Yield x -> map_yield env x

let map_print_statement (env : env) (x : CST.print_statement) =
  match x with
  | `Print_chev_rep_COMMA_exp_opt_COMMA (v1, v2, v3, v4) ->
      let v1 = (* "print" *) token env v1 in
      let v2 = map_chevron env v2 in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_type_ env v2 in
            todo env (v1, v2))
          v3
      in
      let v4 =
        match v4 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ()
      in
      todo env (v1, v2, v3, v4)
  | `Print_exp_rep_COMMA_exp_opt_COMMA (v1, v2, v3, v4) ->
      let v1 = (* "print" *) token env v1 in
      let v2 = map_type_ env v2 in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_type_ env v2 in
            todo env (v1, v2))
          v3
      in
      let v4 =
        match v4 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ()
      in
      todo env (v1, v2, v3, v4)

let map_import_list (env : env) ((v1, v2, v3) : CST.import_list) =
  let v1 = map_anon_choice_dotted_name_c5c573a env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_dotted_name_c5c573a env v2 in
        todo env (v1, v2))
      v2
  in
  let v3 =
    match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

let map_simple_statement (env : env) (x : CST.simple_statement) : stmt =
  match x with
  | `Future_import_stmt (v1, v2, v3, v4) ->
      let v1 = (* "from" *) token env v1 in
      let v2 = (* "__future__" *) token env v2 in
      let v3 = (* "import" *) token env v3 in
      let v4 =
        match v4 with
        | `Import_list x -> map_import_list env x
        | `LPAR_import_list_RPAR (v1, v2, v3) ->
            let v1 = (* "(" *) token env v1 in
            let v2 = map_import_list env v2 in
            let v3 = (* ")" *) token env v3 in
            todo env (v1, v2, v3)
      in
      todo env (v1, v2, v3, v4)
  | `Import_stmt (v1, v2) ->
      let v1 = (* "import" *) token env v1 in
      let v2 = map_import_list env v2 in
      todo env (v1, v2)
  | `Import_from_stmt (v1, v2, v3, v4) ->
      let v1 = (* "from" *) token env v1 in
      let v2 =
        match v2 with
        | `Rela_import x -> map_relative_import env x
        | `Dotted_name x -> map_dotted_name env x
      in
      let v3 = (* "import" *) token env v3 in
      let v4 =
        match v4 with
        | `Wild_import tok -> (* "*" *) token env tok
        | `Import_list x -> map_import_list env x
        | `LPAR_import_list_RPAR (v1, v2, v3) ->
            let v1 = (* "(" *) token env v1 in
            let v2 = map_import_list env v2 in
            let v3 = (* ")" *) token env v3 in
            todo env (v1, v2, v3)
      in
      todo env (v1, v2, v3, v4)
  | `Print_stmt x -> map_print_statement env x
  | `Assert_stmt (v1, v2, v3) ->
      let v1 = (* "assert" *) token env v1 in
      let v2 = map_type_ env v2 in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_type_ env v2 in
            todo env (v1, v2))
          v3
      in
      todo env (v1, v2, v3)
  | `Exp_stmt x ->
      let e = map_expression_statement env x in
      todo env e
  | `Ret_stmt (v1, v2) ->
      let v1 = (* "return" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_expressions env x
        | None -> todo env ()
      in
      todo env (v1, v2)
  | `Delete_stmt (v1, v2) ->
      let v1 = (* "del" *) token env v1 in
      let v2 = map_expressions env v2 in
      todo env (v1, v2)
  | `Raise_stmt (v1, v2, v3) ->
      let v1 = (* "raise" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_expressions env x
        | None -> todo env ()
      in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let v1 = (* "from" *) token env v1 in
            let v2 = map_type_ env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      todo env (v1, v2, v3)
  | `Pass_stmt tok ->
      let t = (* "pass" *) token env tok in
      Pass t
  | `Brk_stmt tok ->
      let t = (* "break" *) token env tok in
      Break t
  | `Cont_stmt tok ->
      let t = (* "continue" *) token env tok in
      Continue t
  | `Global_stmt (v1, v2, v3) ->
      let v1 = (* "global" *) token env v1 in
      let v2 =
        (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) token env v2
      in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 =
              (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) token env v2
            in
            todo env (v1, v2))
          v3
      in
      todo env (v1, v2, v3)
  | `Nonl_stmt (v1, v2, v3) ->
      let v1 = (* "nonlocal" *) token env v1 in
      let v2 =
        (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) token env v2
      in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 =
              (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) token env v2
            in
            todo env (v1, v2))
          v3
      in
      todo env (v1, v2, v3)
  | `Exec_stmt (v1, v2, v3) ->
      let v1 = (* "exec" *) token env v1 in
      let v2 = map_string_ env v2 in
      let v3 =
        match v3 with
        | Some (v1, v2, v3) ->
            let v1 = (* "in" *) token env v1 in
            let v2 = map_type_ env v2 in
            let v3 =
              List.map
                (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = map_type_ env v2 in
                  todo env (v1, v2))
                v3
            in
            todo env (v1, v2, v3)
        | None -> todo env ()
      in
      todo env (v1, v2, v3)

let map_simple_statements (env : env) ((v1, v2, v3, v4) : CST.simple_statements)
    =
  let v1 = map_simple_statement env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* ";" *) token env v1 in
        let v2 = map_simple_statement env v2 in
        todo env (v1, v2))
      v2
  in
  let v3 =
    match v3 with
    | Some tok -> (* ";" *) token env tok
    | None -> todo env ()
  in
  let v4 = (* newline *) token env v4 in
  todo env (v1, v2, v3, v4)

let rec map_block (env : env) ((v1, v2) : CST.block) =
  let v1 = map_module_ env v1 in
  let v2 = (* dedent *) token env v2 in
  todo env (v1, v2)

and map_class_definition (env : env)
    ((v1, v2, v3, v4, v5) : CST.class_definition) : class_definition =
  let v1 = (* "class" *) token env v1 in
  let v2 = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) token env v2 in
  let v3 =
    match v3 with
    | Some x -> map_argument_list env x
    | None -> todo env ()
  in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_suite env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_compound_statement (env : env) (x : CST.compound_statement) : stmt =
  match x with
  | `If_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "if" *) token env v1 in
      let v2 = map_type_ env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_suite env v4 in
      let v5 = List.map (map_elif_clause env) v5 in
      let v6 =
        match v6 with
        | Some x -> map_else_clause env x
        | None -> todo env ()
      in
      todo env (v1, v2, v3, v4, v5, v6)
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 =
        match v1 with
        | Some tok -> (* "async" *) token env tok
        | None -> todo env ()
      in
      let v2 = (* "for" *) token env v2 in
      let v3 = map_left_hand_side env v3 in
      let v4 = (* "in" *) token env v4 in
      let v5 = map_expressions env v5 in
      let v6 = (* ":" *) token env v6 in
      let v7 = map_suite env v7 in
      let v8 =
        match v8 with
        | Some x -> map_else_clause env x
        | None -> todo env ()
      in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `While_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "while" *) token env v1 in
      let v2 = map_type_ env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_suite env v4 in
      let v5 =
        match v5 with
        | Some x -> map_else_clause env x
        | None -> todo env ()
      in
      todo env (v1, v2, v3, v4, v5)
  | `Try_stmt (v1, v2, v3, v4) ->
      let v1 = (* "try" *) token env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_suite env v3 in
      let v4 =
        match v4 with
        | `Rep1_except_clause_opt_else_clause_opt_fina_clause (v1, v2, v3) ->
            let v1 = List.map (map_except_clause env) v1 in
            let v2 =
              match v2 with
              | Some x -> map_else_clause env x
              | None -> todo env ()
            in
            let v3 =
              match v3 with
              | Some x -> map_finally_clause env x
              | None -> todo env ()
            in
            todo env (v1, v2, v3)
        | `Fina_clause x -> map_finally_clause env x
      in
      todo env (v1, v2, v3, v4)
  | `With_stmt (v1, v2, v3, v4, v5) ->
      let v1 =
        match v1 with
        | Some tok -> (* "async" *) token env tok
        | None -> todo env ()
      in
      let v2 = (* "with" *) token env v2 in
      let v3 = map_with_clause env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_suite env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Func_defi x ->
      let def = map_function_definition env x in
      FunctionDef def
  | `Class_defi x ->
      let def = map_class_definition env x in
      ClassDef def
  | `Deco_defi (v1, v2) ->
      let v1 = List.map (map_decorator env) v1 in
      let def =
        match v2 with
        | `Class_defi x ->
            let def = map_class_definition env x in
            ClassDef def
        | `Func_defi x ->
            let def = map_function_definition env x in
            FunctionDef def
      in
      todo env (v1, def)

and map_elif_clause (env : env) ((v1, v2, v3, v4) : CST.elif_clause) =
  let v1 = (* "elif" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_suite env v4 in
  todo env (v1, v2, v3, v4)

and map_else_clause (env : env) ((v1, v2, v3) : CST.else_clause) =
  let v1 = (* "else" *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_suite env v3 in
  todo env (v1, v2, v3)

and map_except_clause (env : env) ((v1, v2, v3, v4) : CST.except_clause) =
  let v1 = (* "except" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_type_ env v1 in
        let v2 =
          match v2 with
          | Some (v1, v2) ->
              let v1 =
                match v1 with
                | `As tok -> (* "as" *) token env tok
                | `COMMA tok -> (* "," *) token env tok
              in
              let v2 = map_type_ env v2 in
              todo env (v1, v2)
          | None -> todo env ()
        in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_suite env v4 in
  todo env (v1, v2, v3, v4)

and map_finally_clause (env : env) ((v1, v2, v3) : CST.finally_clause) =
  let v1 = (* "finally" *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_suite env v3 in
  todo env (v1, v2, v3)

and map_function_definition (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.function_definition) :
    function_definition =
  let v1 =
    match v1 with
    | Some tok -> (* "async" *) token env tok
    | None -> todo env ()
  in
  let v2 = (* "def" *) token env v2 in
  let v3 = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) token env v3 in
  let v4 = map_parameters env v4 in
  let v5 =
    match v5 with
    | Some (v1, v2) ->
        let v1 = (* "->" *) token env v1 in
        let v2 = map_type_ env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v6 = (* ":" *) token env v6 in
  let v7 = map_suite env v7 in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and map_module_ (env : env) (xs : CST.module_) = List.map (map_statement env) xs

and map_statement (env : env) (x : CST.statement) =
  match x with
  | `Simple_stmts x -> map_simple_statements env x
  | `Choice_if_stmt x -> map_compound_statement env x

and map_suite (env : env) (x : CST.suite) =
  match x with
  | `Simple_stmts x -> map_simple_statements env x
  | `Indent_blk (v1, v2) ->
      let v1 = (* indent *) token env v1 in
      let v2 = map_block env v2 in
      todo env (v1, v2)
  | `Nl tok -> (* newline *) token env tok

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_python.Parse.file file)
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      try
        let x = map_module_ env cst in
        failwith "TODO"
      with Failure "not implemented" as exn ->
        H.debug_sexp_cst_after_error (CST.sexp_of_module_ cst);
        raise exn)
