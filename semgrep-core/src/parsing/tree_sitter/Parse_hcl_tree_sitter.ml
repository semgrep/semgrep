(* Yoann Padioleau
 *
 * Copyright (c) 2021 R2C
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
module PI = Parse_info
module CST = Tree_sitter_hcl.CST
module H = Parse_tree_sitter_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* OCaml parser using tree-sitter-lang/semgrep-hcl and converting
 * to the generic AST directly.
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type env = unit H.env

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-cpp/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the hcl CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

let token (env : env) (_tok : Tree_sitter_run.Token.t) =
  failwith "not implemented"

let todo (env : env) _ = failwith "not implemented"

let _map_pat_b66053b (env : env) (tok : CST.pat_b66053b) =
  (* pattern 0x[0-9a-zA-Z]+ *) token env tok

let map_bool_lit (env : env) (x : CST.bool_lit) =
  match x with
  | `True tok -> (* "true" *) token env tok
  | `False tok -> (* "false" *) token env tok

let _map_template_interpolation_end (env : env)
    (tok : CST.template_interpolation_end) =
  (* template_interpolation_end *) token env tok

let _map_pat_e950a1b (env : env) (tok : CST.pat_e950a1b) =
  (* pattern [0-9]+(\.[0-9]+([eE][-+]?[0-9]+)?)? *) token env tok

let map_heredoc_start (env : env) (x : CST.heredoc_start) =
  match x with
  | `LTLT tok -> (* "<<" *) token env tok
  | `LTLTDASH tok -> (* "<<-" *) token env tok

let _map_template_literal_chunk (env : env) (tok : CST.template_literal_chunk) =
  (* template_literal_chunk *) token env tok

let _map_identifier (env : env) (tok : CST.identifier) =
  (* identifier *) token env tok

let _map_quoted_template_start (env : env) (tok : CST.quoted_template_start) =
  (* quoted_template_start *) token env tok

let _map_template_interpolation_start (env : env)
    (tok : CST.template_interpolation_start) =
  (* template_interpolation_start *) token env tok

let _map_ellipsis (env : env) (tok : CST.ellipsis) =
  (* ellipsis *) token env tok

let _map_heredoc_identifier (env : env) (tok : CST.heredoc_identifier) =
  (* heredoc_identifier *) token env tok

let map_template_directive (env : env) (x : CST.template_directive) =
  match x with
  | `PERC_5eef7bb tok -> (* "%{if TODO" *) token env tok
  | `PERC_58c37dd tok -> (* "%{for TODO" *) token env tok

let map_numeric_lit (env : env) (x : CST.numeric_lit) =
  match x with
  | `Pat_e950a1b tok ->
      (* pattern [0-9]+(\.[0-9]+([eE][-+]?[0-9]+)?)? *) token env tok
  | `Pat_b66053b tok -> (* pattern 0x[0-9a-zA-Z]+ *) token env tok

let map_template_literal (env : env) (xs : CST.template_literal) =
  List.map (token env (* template_literal_chunk *)) xs

let map_string_lit (env : env) ((v1, v2, v3) : CST.string_lit) =
  let v1 = (* quoted_template_start *) token env v1 in
  let v2 = map_template_literal env v2 in
  let v3 = (* quoted_template_end *) token env v3 in
  todo env (v1, v2, v3)

let map_get_attr (env : env) ((v1, v2) : CST.get_attr) =
  let v1 = (* "." *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  todo env (v1, v2)

let map_literal_value (env : env) (x : CST.literal_value) =
  match x with
  | `Nume_lit x -> map_numeric_lit env x
  | `Bool_lit x -> map_bool_lit env x
  | `Null_lit tok -> (* "null" *) token env tok
  | `Str_lit x -> map_string_lit env x

let rec map_anon_choice_get_attr_7bbf24f (env : env)
    (x : CST.anon_choice_get_attr_7bbf24f) =
  match x with `Get_attr x -> map_get_attr env x | `Index x -> map_index env x

and map_anon_choice_temp_lit_c764a73 (env : env)
    (x : CST.anon_choice_temp_lit_c764a73) =
  match x with
  | `Temp_lit x -> map_template_literal env x
  | `Temp_interp (v1, v2, v3, v4, v5) ->
      let v1 = (* template_interpolation_start *) token env v1 in
      let v2 =
        match v2 with
        | Some tok -> (* "~" *) token env tok
        | None -> todo env ()
      in
      let v3 =
        match v3 with Some x -> map_expression env x | None -> todo env ()
      in
      let v4 =
        match v4 with
        | Some tok -> (* "~" *) token env tok
        | None -> todo env ()
      in
      let v5 = (* template_interpolation_end *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Temp_dire x -> map_template_directive env x

and map_binary_operation (env : env) (x : CST.binary_operation) =
  match x with
  | `Expr_term_choice_STAR_expr_term (v1, v2, v3) ->
      let v1 = map_expr_term env v1 in
      let v2 =
        match v2 with
        | `STAR tok -> (* "*" *) token env tok
        | `SLASH tok -> (* "/" *) token env tok
        | `PERC tok -> (* "%" *) token env tok
      in
      let v3 = map_expr_term env v3 in
      todo env (v1, v2, v3)
  | `Expr_term_choice_PLUS_expr_term (v1, v2, v3) ->
      let v1 = map_expr_term env v1 in
      let v2 =
        match v2 with
        | `PLUS tok -> (* "+" *) token env tok
        | `DASH tok -> (* "-" *) token env tok
      in
      let v3 = map_expr_term env v3 in
      todo env (v1, v2, v3)
  | `Expr_term_choice_GT_expr_term (v1, v2, v3) ->
      let v1 = map_expr_term env v1 in
      let v2 =
        match v2 with
        | `GT tok -> (* ">" *) token env tok
        | `GTEQ tok -> (* ">=" *) token env tok
        | `LT tok -> (* "<" *) token env tok
        | `LTEQ tok -> (* "<=" *) token env tok
      in
      let v3 = map_expr_term env v3 in
      todo env (v1, v2, v3)
  | `Expr_term_choice_EQEQ_expr_term (v1, v2, v3) ->
      let v1 = map_expr_term env v1 in
      let v2 =
        match v2 with
        | `EQEQ tok -> (* "==" *) token env tok
        | `BANGEQ tok -> (* "!=" *) token env tok
      in
      let v3 = map_expr_term env v3 in
      todo env (v1, v2, v3)
  | `Expr_term_choice_AMPAMP_expr_term (v1, v2, v3) ->
      let v1 = map_expr_term env v1 in
      let v2 = match v2 with `AMPAMP tok -> (* "&&" *) token env tok in
      let v3 = map_expr_term env v3 in
      todo env (v1, v2, v3)
  | `Expr_term_choice_BARBAR_expr_term (v1, v2, v3) ->
      let v1 = map_expr_term env v1 in
      let v2 = match v2 with `BARBAR tok -> (* "||" *) token env tok in
      let v3 = map_expr_term env v3 in
      todo env (v1, v2, v3)

and map_collection_value (env : env) (x : CST.collection_value) =
  match x with
  | `Tuple (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        match v2 with Some x -> map_tuple_elems env x | None -> todo env ()
      in
      let v3 = (* "]" *) token env v3 in
      todo env (v1, v2, v3)
  | `Obj x -> map_object_ env x

and map_expr_term (env : env) (x : CST.expr_term) =
  match x with
  | `Lit_value x -> map_literal_value env x
  | `Temp_expr x -> map_template_expr env x
  | `Coll_value x -> map_collection_value env x
  | `Var_expr tok -> (* identifier *) token env tok
  | `Func_call (v1, v2, v3, v4) ->
      let v1 = (* identifier *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> map_function_arguments env x
        | None -> todo env ()
      in
      let v4 = (* ")" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `For_expr x -> map_for_expr env x
  | `Oper x -> map_operation env x
  | `Expr_term_index (v1, v2) ->
      let v1 = map_expr_term env v1 in
      let v2 = map_index env v2 in
      todo env (v1, v2)
  | `Expr_term_get_attr (v1, v2) ->
      let v1 = map_expr_term env v1 in
      let v2 = map_get_attr env v2 in
      todo env (v1, v2)
  | `Expr_term_splat (v1, v2) ->
      let v1 = map_expr_term env v1 in
      let v2 = map_splat env v2 in
      todo env (v1, v2)
  | `LPAR_exp_RPAR (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      todo env (v1, v2, v3)

and map_expression (env : env) (x : CST.expression) =
  match x with
  | `Expr_term x -> map_expr_term env x
  | `Cond (v1, v2, v3, v4, v5) ->
      let v1 = map_expression env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      todo env (v1, v2, v3, v4, v5)

and map_for_cond (env : env) ((v1, v2) : CST.for_cond) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_for_expr (env : env) (x : CST.for_expr) =
  match x with
  | `For_tuple_expr (v1, v2, v3, v4, v5) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = map_for_intro env v2 in
      let v3 = map_expression env v3 in
      let v4 =
        match v4 with Some x -> map_for_cond env x | None -> todo env ()
      in
      let v5 = (* "]" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `For_obj_expr (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = (* "{" *) token env v1 in
      let v2 = map_for_intro env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "=>" *) token env v4 in
      let v5 = map_expression env v5 in
      let v6 =
        match v6 with
        | Some tok -> (* ellipsis *) token env tok
        | None -> todo env ()
      in
      let v7 =
        match v7 with Some x -> map_for_cond env x | None -> todo env ()
      in
      let v8 = (* "}" *) token env v8 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)

and map_for_intro (env : env) ((v1, v2, v3, v4, v5, v6) : CST.for_intro) =
  let v1 = (* "for" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = (* identifier *) token env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v4 = (* "in" *) token env v4 in
  let v5 = map_expression env v5 in
  let v6 = (* ":" *) token env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and map_function_arguments (env : env) ((v1, v2, v3) : CST.function_arguments) =
  let v1 = map_expression env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_expression env v2 in
        todo env (v1, v2))
      v2
  in
  let v3 =
    match v3 with
    | Some x -> (
        match x with
        | `COMMA tok -> (* "," *) token env tok
        | `Ellips tok -> (* ellipsis *) token env tok)
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_index (env : env) (x : CST.index) =
  match x with
  | `New_index (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "]" *) token env v3 in
      todo env (v1, v2, v3)
  | `Legacy_index (v1, v2) ->
      let v1 = (* "." *) token env v1 in
      let v2 = (* pattern [0-9]+ *) token env v2 in
      todo env (v1, v2)

and map_object_ (env : env) ((v1, v2, v3) : CST.object_) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    match v2 with Some x -> map_object_elems env x | None -> todo env ()
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_object_elem (env : env) ((v1, v2, v3) : CST.object_elem) =
  let v1 = map_expression env v1 in
  let v2 =
    match v2 with
    | `EQ tok -> (* "=" *) token env tok
    | `COLON tok -> (* ":" *) token env tok
  in
  let v3 = map_expression env v3 in
  todo env (v1, v2, v3)

and map_object_elems (env : env) ((v1, v2, v3) : CST.object_elems) =
  let v1 = map_object_elem env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 =
          match v1 with
          | Some tok -> (* "," *) token env tok
          | None -> todo env ()
        in
        let v2 = map_object_elem env v2 in
        todo env (v1, v2))
      v2
  in
  let v3 =
    match v3 with Some tok -> (* "," *) token env tok | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_operation (env : env) (x : CST.operation) =
  match x with
  | `Un_oper (v1, v2) ->
      let v1 =
        match v1 with
        | `DASH tok -> (* "-" *) token env tok
        | `BANG tok -> (* "!" *) token env tok
      in
      let v2 = map_expr_term env v2 in
      todo env (v1, v2)
  | `Bin_oper x -> map_binary_operation env x

and map_splat (env : env) (x : CST.splat) =
  match x with
  | `Attr_splat (v1, v2) ->
      let v1 = (* ".*" *) token env v1 in
      let v2 = List.map (map_anon_choice_get_attr_7bbf24f env) v2 in
      todo env (v1, v2)
  | `Full_splat (v1, v2) ->
      let v1 = (* "[*]" *) token env v1 in
      let v2 = List.map (map_anon_choice_get_attr_7bbf24f env) v2 in
      todo env (v1, v2)

and map_template_expr (env : env) (x : CST.template_expr) =
  match x with
  | `Quoted_temp (v1, v2, v3) ->
      let v1 = (* quoted_template_start *) token env v1 in
      let v2 =
        match v2 with
        | Some xs -> List.map (map_anon_choice_temp_lit_c764a73 env) xs
        | None -> todo env ()
      in
      let v3 = (* quoted_template_end *) token env v3 in
      todo env (v1, v2, v3)
  | `Here_temp (v1, v2, v3, v4) ->
      let v1 = map_heredoc_start env v1 in
      let v2 = (* heredoc_identifier *) token env v2 in
      let v3 =
        match v3 with
        | Some xs -> List.map (map_anon_choice_temp_lit_c764a73 env) xs
        | None -> todo env ()
      in
      let v4 = (* heredoc_identifier *) token env v4 in
      todo env (v1, v2, v3, v4)

and map_tuple_elems (env : env) ((v1, v2, v3) : CST.tuple_elems) =
  let v1 = map_expression env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_expression env v2 in
        todo env (v1, v2))
      v2
  in
  let v3 =
    match v3 with Some tok -> (* "," *) token env tok | None -> todo env ()
  in
  todo env (v1, v2, v3)

let map_attribute (env : env) ((v1, v2, v3) : CST.attribute) =
  let v1 = (* identifier *) token env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_expression env v3 in
  todo env (v1, v2, v3)

let rec map_block (env : env) ((v1, v2, v3, v4, v5) : CST.block) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    List.map
      (fun x ->
        match x with
        | `Str_lit x -> map_string_lit env x
        | `Id tok -> (* identifier *) token env tok)
      v2
  in
  let v3 = (* "{" *) token env v3 in
  let v4 = match v4 with Some x -> map_body env x | None -> todo env () in
  let v5 = (* "}" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_body (env : env) (xs : CST.body) =
  List.map
    (fun x ->
      match x with `Attr x -> map_attribute env x | `Blk x -> map_block env x)
    xs

let map_config_file (env : env) (opt : CST.config_file) =
  match opt with
  | Some x -> (
      match x with `Body x -> map_body env x | `Obj x -> map_object_ env x)
  | None -> todo env ()

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_hcl.Parse.file file)
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      try map_config_file env cst
      with Failure "not implemented" as exn ->
        H.debug_sexp_cst_after_error (CST.sexp_of_config_file cst);
        raise exn)
