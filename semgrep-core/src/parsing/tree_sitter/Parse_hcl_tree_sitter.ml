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
open Common
module PI = Parse_info
module CST = Tree_sitter_hcl.CST
module H = Parse_tree_sitter_helpers
open AST_generic
module G = AST_generic
module H2 = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* OCaml parser using tree-sitter-lang/semgrep-hcl and converting
 * to the generic AST directly.
 *
 * See https://www.terraform.io/docs/language/ for more information
 * on the HCL language (a.k.a terraform).
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type env = unit H.env

let token = H.token

let str = H.str

(* for list/dict comprehensions *)
let pattern_of_ids ids =
  match ids with
  (* actually in HCL there are either 1 or 2 elts *)
  | [] -> raise Impossible
  | [ id ] -> PatId (id, empty_id_info ()) |> G.p
  | _ ->
      let xs =
        ids |> List.map (fun id -> PatId (id, empty_id_info ()) |> G.p)
      in
      PatTuple (fake_bracket xs) |> G.p

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-cpp/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the hcl CST
   to another type of tree.
*)

let map_bool_lit (env : env) (x : CST.bool_lit) : bool wrap =
  match x with
  | `True tok -> (* "true" *) (true, token env tok)
  | `False tok -> (* "false" *) (false, token env tok)

let map_heredoc_start (env : env) (x : CST.heredoc_start) =
  match x with
  | `LTLT tok -> (* "<<" *) token env tok
  | `LTLTDASH tok -> (* "<<-" *) token env tok

let map_template_directive (env : env) (x : CST.template_directive) =
  match x with
  | `PERC_5eef7bb tok -> (* "%{if TODO" *) token env tok
  | `PERC_58c37dd tok -> (* "%{for TODO" *) token env tok

let map_numeric_lit (env : env) (x : CST.numeric_lit) : literal =
  match x with
  | `Pat_e950a1b tok ->
      (* pattern [0-9]+(\.[0-9]+([eE][-+]?[0-9]+)?)? *)
      let x = str env tok in
      H.parse_number_literal x
  | `Pat_b66053b tok ->
      (* pattern 0x[0-9a-zA-Z]+ *)
      let x = str env tok in
      H.parse_number_literal x

let map_template_literal (env : env) (xs : CST.template_literal) :
    string wrap option =
  (* each character is a separate template_literal_chunk, so we need
   * to merge them in a single string.
   *)
  let xs = List.map (str env (* template_literal_chunk *)) xs in
  match xs with
  | [] -> None
  | (s1, t1) :: ys ->
      let str = List.map fst ys |> String.concat "" in
      let toks = List.map snd ys in
      Some (s1 ^ str, PI.combine_infos t1 toks)

let map_string_lit (env : env) ((v1, v2, v3) : CST.string_lit) =
  let v1 = (* quoted_template_start *) token env v1 in
  let v2 = map_template_literal env v2 in
  let v3 = (* quoted_template_end *) token env v3 in
  match v2 with
  | Some (s, t) -> G.String (s, PI.combine_infos v1 [ t; v3 ])
  | None -> G.String ("", PI.combine_infos v1 [ v3 ])

let map_get_attr (env : env) ((v1, v2) : CST.get_attr) =
  let v1 = (* "." *) token env v1 in
  let v2 = (* identifier *) str env v2 in
  fun e ->
    let n = H2.name_of_id v2 in
    G.DotAccess (e, v1, EN n) |> G.e

let map_literal_value (env : env) (x : CST.literal_value) : literal =
  match x with
  | `Nume_lit x -> map_numeric_lit env x
  | `Bool_lit x -> Bool (map_bool_lit env x)
  | `Null_lit tok -> (* "null" *) Null (token env tok)
  | `Str_lit x -> map_string_lit env x

let rec map_anon_choice_get_attr_7bbf24f (env : env)
    (x : CST.anon_choice_get_attr_7bbf24f) =
  match x with `Get_attr x -> map_get_attr env x | `Index x -> map_index env x

and map_anon_choice_temp_lit_c764a73 (env : env)
    (x : CST.anon_choice_temp_lit_c764a73) =
  match x with
  | `Temp_lit x ->
      let sopt = map_template_literal env x in
      sopt |> Common.opt_to_list |> List.map (fun s -> Left3 s)
  | `Temp_interp (v1, v2, v3, v4, v5) ->
      let v1 = (* template_interpolation_start *) token env v1 in
      (* TODO: what is this ~? *)
      let _v2TODO =
        match v2 with
        | Some tok -> (* "~" *) Some (token env tok)
        | None -> None
      in
      let v3 =
        match v3 with Some x -> Some (map_expression env x) | None -> None
      in
      let _v4TODO =
        match v4 with
        | Some tok -> (* "~" *) Some (token env tok)
        | None -> None
      in
      let v5 = (* template_interpolation_end *) token env v5 in
      [ Right3 (v1, v3, v5) ]
  | `Temp_dire x ->
      let t = map_template_directive env x in
      [ Middle3 (G.OtherExpr (OE_Todo, [ TodoK ("directive", t) ]) |> G.e) ]

and map_binary_operation (env : env) (x : CST.binary_operation) =
  match x with
  | `Expr_term_choice_STAR_expr_term (v1, v2, v3) ->
      let v1 = map_expr_term env v1 in
      let v2 =
        match v2 with
        | `STAR tok -> (* "*" *) (Mult, token env tok)
        | `SLASH tok -> (* "/" *) (Div, token env tok)
        | `PERC tok -> (* "%" *) (Mod, token env tok)
      in
      let v3 = map_expr_term env v3 in
      (v1, v2, v3)
  | `Expr_term_choice_PLUS_expr_term (v1, v2, v3) ->
      let v1 = map_expr_term env v1 in
      let v2 =
        match v2 with
        | `PLUS tok -> (* "+" *) (Plus, token env tok)
        | `DASH tok -> (* "-" *) (Minus, token env tok)
      in
      let v3 = map_expr_term env v3 in
      (v1, v2, v3)
  | `Expr_term_choice_GT_expr_term (v1, v2, v3) ->
      let v1 = map_expr_term env v1 in
      let v2 =
        match v2 with
        | `GT tok -> (* ">" *) (Gt, token env tok)
        | `GTEQ tok -> (* ">=" *) (GtE, token env tok)
        | `LT tok -> (* "<" *) (Lt, token env tok)
        | `LTEQ tok -> (* "<=" *) (LtE, token env tok)
      in
      let v3 = map_expr_term env v3 in
      (v1, v2, v3)
  | `Expr_term_choice_EQEQ_expr_term (v1, v2, v3) ->
      let v1 = map_expr_term env v1 in
      let v2 =
        match v2 with
        | `EQEQ tok -> (* "==" *) (Eq, token env tok)
        | `BANGEQ tok -> (* "!=" *) (NotEq, token env tok)
      in
      let v3 = map_expr_term env v3 in
      (v1, v2, v3)
  | `Expr_term_choice_AMPAMP_expr_term (v1, v2, v3) ->
      let v1 = map_expr_term env v1 in
      let v2 = match v2 with `AMPAMP tok -> (* "&&" *) (And, token env tok) in
      let v3 = map_expr_term env v3 in
      (v1, v2, v3)
  | `Expr_term_choice_BARBAR_expr_term (v1, v2, v3) ->
      let v1 = map_expr_term env v1 in
      let v2 = match v2 with `BARBAR tok -> (* "||" *) (Or, token env tok) in
      let v3 = map_expr_term env v3 in
      (v1, v2, v3)

and map_collection_value (env : env) (x : CST.collection_value) : expr =
  match x with
  | `Tuple (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = match v2 with Some x -> map_tuple_elems env x | None -> [] in
      let v3 = (* "]" *) token env v3 in
      G.Tuple (v1, v2, v3) |> G.e
  | `Obj x -> map_object_ env x

and map_expr_term (env : env) (x : CST.expr_term) : expr =
  match x with
  | `Lit_value x -> L (map_literal_value env x) |> G.e
  | `Temp_expr x -> map_template_expr env x
  | `Coll_value x -> map_collection_value env x
  | `Var_expr tok ->
      (* identifier *)
      let id = str env tok in
      N (H2.name_of_id id) |> G.e
  | `Func_call (v1, v2, v3, v4) ->
      let v1 = (* identifier *) str env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with Some x -> map_function_arguments env x | None -> []
      in
      let v4 = (* ")" *) token env v4 in
      let n = N (H2.name_of_id v1) |> G.e in
      Call (n, (v2, v3, v4)) |> G.e
  | `For_expr x -> map_for_expr env x
  | `Oper x -> map_operation env x
  | `Expr_term_index (v1, v2) ->
      let v1 = map_expr_term env v1 in
      let v2 = map_index env v2 in
      v2 v1
  | `Expr_term_get_attr (v1, v2) ->
      let v1 = map_expr_term env v1 in
      let v2 = map_get_attr env v2 in
      v2 v1
  | `Expr_term_splat (v1, v2) ->
      let v1 = map_expr_term env v1 in
      let v2 = map_splat env v2 in
      v2 v1
  | `LPAR_exp_RPAR (v1, v2, v3) ->
      let _v1 = (* "(" *) token env v1 in
      let v2 = map_expression env v2 in
      let _v3 = (* ")" *) token env v3 in
      v2

and map_expression (env : env) (x : CST.expression) : expr =
  match x with
  | `Expr_term x -> map_expr_term env x
  | `Cond (v1, v2, v3, v4, v5) ->
      let v1 = map_expression env v1 in
      let _v2 = (* "?" *) token env v2 in
      let v3 = map_expression env v3 in
      let _v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      Conditional (v1, v3, v5) |> G.e

and map_for_cond (env : env) ((v1, v2) : CST.for_cond) : for_or_if_comp =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_expression env v2 in
  CompIf (v1, v2)

and map_for_expr (env : env) (x : CST.for_expr) =
  match x with
  | `For_tuple_expr (v1, v2, v3, v4, v5) ->
      let v1 = (* "[" *) token env v1 in
      let tfor, ids, tin, e, _tcolon = map_for_intro env v2 in
      let v3 = map_expression env v3 in
      let v4 = match v4 with Some x -> [ map_for_cond env x ] | None -> [] in
      let v5 = (* "]" *) token env v5 in
      let pat = pattern_of_ids ids in
      let compfor = CompFor (tfor, pat, tin, e) in
      let xs = compfor :: v4 in
      Comprehension (TupleComprehension, (v1, (v3, xs), v5)) |> G.e
  | `For_obj_expr (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = (* "{" *) token env v1 in
      let tfor, ids, tin, e, _tcolon = map_for_intro env v2 in
      let v3 = map_expression env v3 in
      let _v4 = (* "=>" *) token env v4 in
      let v5 = map_expression env v5 in
      (* ??? *)
      let _v6TODO =
        match v6 with
        | Some tok -> (* ellipsis *) Some (token env tok)
        | None -> None
      in
      let v7 = match v7 with Some x -> [ map_for_cond env x ] | None -> [] in
      let v8 = (* "}" *) token env v8 in
      let pat = pattern_of_ids ids in
      let compfor = CompFor (tfor, pat, tin, e) in
      let xs = compfor :: v7 in
      let ekeyval = Tuple (fake_bracket [ v3; v5 ]) |> G.e in
      Comprehension (Dict, (v1, (ekeyval, xs), v8)) |> G.e

and map_for_intro (env : env) ((v1, v2, v3, v4, v5, v6) : CST.for_intro) =
  let v1 = (* "for" *) token env v1 in
  let v2 = (* identifier *) str env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = (* identifier *) str env v2 in
        [ v2 ]
    | None -> []
  in
  let v4 = (* "in" *) token env v4 in
  let v5 = map_expression env v5 in
  let v6 = (* ":" *) token env v6 in
  (v1, v2 :: v3, v4, v5, v6)

and map_function_arguments (env : env) ((v1, v2, v3) : CST.function_arguments) :
    argument list =
  let v1 = map_expression env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_expression env v2 in
        Arg v2)
      v2
  in
  let v3 =
    match v3 with
    | Some x -> (
        match x with
        | `COMMA tok ->
            (* "," *)
            let _t = token env tok in
            []
        | `Ellips tok ->
            (* ellipsis *)
            let t = token env tok in
            [ Arg (Ellipsis t |> G.e) ])
    | None -> []
  in
  [ Arg v1 ] @ v2 @ v3

and map_index (env : env) (x : CST.index) =
  match x with
  | `New_index (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "]" *) token env v3 in
      fun e -> G.ArrayAccess (e, (v1, v2, v3)) |> G.e
  | `Legacy_index (v1, v2) ->
      let v1 = (* "." *) token env v1 in
      let v2 = (* pattern [0-9]+ *) str env v2 in
      let idx = L (H.parse_number_literal v2) |> G.e in
      fun e -> G.ArrayAccess (e, (v1, idx, v1)) |> G.e

and map_object_ (env : env) ((v1, v2, v3) : CST.object_) =
  let v1 = (* "{" *) token env v1 in
  let v2 = match v2 with Some x -> map_object_elems env x | None -> [] in
  let v3 = (* "}" *) token env v3 in
  Record (v1, v2, v3) |> G.e

and map_object_elem (env : env) ((v1, v2, v3) : CST.object_elem) : field =
  let v1 = map_expression env v1 in
  let v2 =
    match v2 with
    | `EQ tok -> (* "=" *) Left (token env tok)
    | `COLON tok -> (* ":" *) Right (token env tok)
  in
  let v3 = map_expression env v3 in
  let n_or_dyn = match v1.e with N n -> EN n | _ -> EDynamic v1 in
  let ent = { name = n_or_dyn; attrs = []; tparams = [] } in
  let vdef = { vinit = Some v3; vtype = None } in
  let def =
    match v2 with
    | Left _teq -> VarDef vdef
    | Right _tcolon -> FieldDefColon vdef
  in
  (ent, def) |> G.fld

and map_object_elems (env : env) ((v1, v2, v3) : CST.object_elems) =
  let v1 = map_object_elem env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          match v1 with
          | Some tok -> (* "," *) Some (token env tok)
          | None -> None
        in
        let v2 = map_object_elem env v2 in
        v2)
      v2
  in
  let _v3 =
    match v3 with Some tok -> (* "," *) Some (token env tok) | None -> None
  in
  v1 :: v2

and map_operation (env : env) (x : CST.operation) : expr =
  match x with
  | `Un_oper (v1, v2) ->
      let op, t =
        match v1 with
        | `DASH tok -> (* "-" *) (Minus, token env tok)
        | `BANG tok -> (* "!" *) (Not, token env tok)
      in
      let v2 = map_expr_term env v2 in
      Call (IdSpecial (Op op, t) |> G.e, fake_bracket [ Arg v2 ]) |> G.e
  | `Bin_oper x ->
      let a, (op, t), c = map_binary_operation env x in
      Call (IdSpecial (Op op, t) |> G.e, fake_bracket [ Arg a; Arg c ]) |> G.e

and map_splat (env : env) (x : CST.splat) =
  match x with
  | `Attr_splat (v1, v2) ->
      let v1 = (* ".*" *) token env v1 in
      let f1 e =
        let access = EDynamic (IdSpecial (HashSplat, v1) |> G.e) in
        DotAccess (e, v1, access) |> G.e
      in
      let v2 = List.map (map_anon_choice_get_attr_7bbf24f env) v2 in
      fun e -> v2 |> List.fold_left (fun acc f -> f acc) (f1 e)
  | `Full_splat (v1, v2) ->
      let v1 = (* "[*]" *) token env v1 in
      let f1 e =
        let access = IdSpecial (HashSplat, v1) |> G.e in
        ArrayAccess (e, (v1, access, v1)) |> G.e
      in
      let v2 = List.map (map_anon_choice_get_attr_7bbf24f env) v2 in
      fun e -> v2 |> List.fold_left (fun acc f -> f acc) (f1 e)

and map_template_expr (env : env) (x : CST.template_expr) =
  match x with
  | `Quoted_temp (v1, v2, v3) ->
      let v1 = (* quoted_template_start *) token env v1 in
      let v2 =
        match v2 with
        | Some xs ->
            List.map (map_anon_choice_temp_lit_c764a73 env) xs |> List.flatten
        | None -> []
      in
      let v3 = (* quoted_template_end *) token env v3 in
      G.interpolated (v1, v2, v3)
  | `Here_temp (v1, v2, v3, v4) ->
      let v1 = map_heredoc_start env v1 in
      let v2 = (* heredoc_identifier *) token env v2 in
      let v3 =
        match v3 with
        | Some xs ->
            List.map (map_anon_choice_temp_lit_c764a73 env) xs |> List.flatten
        | None -> []
      in
      let v4 = (* heredoc_identifier *) token env v4 in
      let t1 = PI.combine_infos v1 [ v2 ] in
      G.interpolated (t1, v3, v4)

and map_tuple_elems (env : env) ((v1, v2, v3) : CST.tuple_elems) : expr list =
  let v1 = map_expression env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_expression env v2 in
        v2)
      v2
  in
  let _v3 =
    match v3 with Some tok -> (* "," *) Some (token env tok) | None -> None
  in
  v1 :: v2

let map_attribute (env : env) ((v1, v2, v3) : CST.attribute) : definition =
  let v1 = (* identifier *) str env v1 in
  let _v2 = (* "=" *) token env v2 in
  let v3 = map_expression env v3 in
  let ent = G.basic_entity v1 [] in
  let def = { vinit = Some v3; vtype = None } in
  (ent, VarDef def)

(* TODO? convert to a definition? a class_def? *)
let rec map_block (env : env) ((v1, v2, v3, v4, v5) : CST.block) : G.expr =
  (* TODO? usually 'resource', 'locals', 'variable', other? *)
  let v1 = (* identifier *) str env v1 in
  let v2 =
    List.map
      (fun x ->
        match x with
        | `Str_lit x ->
            let x = map_string_lit env x in
            L x |> G.e
        | `Id tok ->
            let id = (* identifier *) str env tok in
            let n = H2.name_of_id id in
            N n |> G.e)
      v2
  in
  let v3 = (* "{" *) token env v3 in
  let v4 = match v4 with Some x -> map_body env x | None -> [] in
  let v5 = (* "}" *) token env v5 in

  let n = H2.name_of_id v1 in
  (* convert in a Record like map_object *)
  let flds = v4 |> List.map (fun st -> FieldStmt st) in
  let body = Record (v3, flds, v5) |> G.e in
  let es = [ N n |> G.e ] @ v2 @ [ body ] in
  let args = es |> List.map G.arg in
  (* TODO? convert in something else? *)
  let special = IdSpecial (New, snd v1) |> G.e in
  G.Call (special, fake_bracket args) |> G.e

(* TODO? convert to a field, to be similar to map_object_, so maybe some
 * object semgrep pattern can also match block body.
 *)
and map_body (env : env) (xs : CST.body) : item list =
  List.map
    (fun x ->
      match x with
      | `Attr x ->
          let def = map_attribute env x in
          DefStmt def |> G.s
      | `Blk x ->
          let blk = map_block env x in
          G.exprstmt blk)
    xs

let map_config_file (env : env) (opt : CST.config_file) : program =
  match opt with
  | Some x -> (
      match x with
      | `Body x ->
          let bd = map_body env x in
          bd
      | `Obj x ->
          let x = map_object_ env x in
          [ G.exprstmt x ])
  | None -> []

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
