(* Yoann Padioleau
 *
 * Copyright (c) 2021, 2023 r2c
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
open Common
open Either_
open Fpath_.Operators
module CST = Tree_sitter_hcl.CST
module H = Parse_tree_sitter_helpers
open AST_terraform
module G = AST_generic
module H2 = AST_generic_helpers
module Loc = Tree_sitter_run.Loc

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
let fb = Tok.unsafe_fake_bracket

(* for list/dict comprehensions *)
let pattern_of_ids ids =
  match ids with
  (* actually in HCL there are either 1 or 2 elts *)
  | [] -> raise Impossible
  | [ id ] -> G.PatId (id, G.empty_id_info ()) |> G.p
  | _ ->
      let xs =
        ids |> List_.map (fun id -> G.PatId (id, G.empty_id_info ()) |> G.p)
      in
      G.PatTuple (fb xs) |> G.p

(* val parse_number_literal : string * Parse_info.t -> AST_generic.literal *)
let parse_number_literal (s, t) =
  match Parsed_int.parse_c_octal (s, t) with
  | (Some _, _) as pi -> G.Int pi
  | pi -> (
      match float_of_string_opt s with
      | Some f -> G.Float (Some f, t)
      | None -> G.Int pi)

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

let map_numeric_lit (env : env) (x : CST.numeric_lit) : G.literal =
  match x with
  | `Pat_e950a1b tok ->
      (* pattern [0-9]+(\.[0-9]+([eE][-+]?[0-9]+)?)? *)
      let x = str env tok in
      parse_number_literal x
  | `Pat_b66053b tok ->
      (* pattern 0x[0-9a-zA-Z]+ *)
      let x = str env tok in
      parse_number_literal x

(* This is supposed to be a string, but we get a list of non-whitespace
 * characters ('template_literal_chunk's or 'Token.t's), so we need to
 * reconstruct the string from them.
 *
 * Note that we need to respect newlines and spaces so that HEREDOCs are
 * correctly parsed, this is important e.g. if you later want to
 * analyze the string with metavariable-regex or metavariable-pattern
 * (see tests/rules/terraform_nested_yaml).
 *
 * TODO: Could we handle this properly already in the Tree-sitter parser? *)
let map_template_literal (env : env) (xs : CST.template_literal) :
    string wrap option =
  match xs with
  | [] -> None
  | ((x_pos, x_str) as x) :: xs ->
      let base_col = x_pos.Loc.start.column in
      let rec concat_chunks acc_str prev_row prev_col xs =
        match xs with
        | [] -> acc_str
        | x :: xs ->
            let x_loc, x_str = x in
            let prev_col =
              if x_loc.Loc.start.row > prev_row then base_col else prev_col
            in
            let acc_str =
              acc_str
              ^ String.make (max 0 (x_loc.Loc.start.row - prev_row)) '\n'
              ^ String.make (max 0 (x_loc.Loc.start.column - prev_col)) ' '
              ^ x_str
            in
            concat_chunks acc_str x_loc.Loc.end_.row x_loc.Loc.end_.column xs
      in
      let str =
        concat_chunks x_str x_pos.Loc.end_.row x_pos.Loc.end_.column xs
      in
      let tok = Tok.rewrap_str str (token env x) in
      Some (str, tok)

let map_identifier (env : env) (x : CST.identifier) : ident =
  match x with
  | `Tok_choice_pat_3e8fcfc_rep_choice_pat_71519dc tok ->
      (* tok_choice_pat_3e8fcfc_rep_choice_pat_71519dc *) str env tok
  | `Semg_meta tok -> (* semgrep_metavariable *) str env tok

let map_string_lit (env : env) ((v1, v2, v3) : CST.string_lit) :
    string wrap bracket =
  let l = (* quoted_template_start *) token env v1 in
  let xs = map_template_literal env v2 |> Option.to_list in
  let r = (* quoted_template_end *) token env v3 in
  G.string_ (l, xs, r)

let map_variable_expr (env : env) (x : CST.variable_expr) = map_identifier env x

let map_get_attr (env : env) ((v1, v2) : CST.get_attr) =
  let v1 = (* "." *) token env v1 in
  let v2 = map_variable_expr env v2 in
  fun e ->
    let n = H2.name_of_id v2 in
    G.DotAccess (e, v1, FN n) |> G.e

let map_literal_value (env : env) (x : CST.literal_value) : G.literal =
  match x with
  | `Nume_lit x -> map_numeric_lit env x
  | `Bool_lit x -> G.Bool (map_bool_lit env x)
  | `Null_lit tok -> (* "null" *) G.Null (token env tok)
  | `Str_lit x -> G.String (map_string_lit env x)

let rec map_anon_choice_get_attr_7bbf24f (env : env)
    (x : CST.anon_choice_get_attr_7bbf24f) =
  match x with
  | `Get_attr x -> map_get_attr env x
  | `Index x -> map_index env x

and map_anon_choice_temp_lit_c764a73 (env : env)
    (x : CST.anon_choice_temp_lit_0082c06) =
  match x with
  | `Temp_lit x ->
      let sopt = map_template_literal env x in
      sopt |> Option.to_list |> List_.map (fun s -> Left3 s)
  | `Temp_interp (v1, v2, v3, v4, v5) ->
      let v1 = (* template_interpolation_start *) token env v1 in
      (* TODO: what is this ~? *)
      let _v2TODO =
        match v2 with
        | Some tok -> (* "~" *) Some (token env tok)
        | None -> None
      in
      let v3 =
        match v3 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      let _v4TODO =
        match v4 with
        | Some tok -> (* "~" *) Some (token env tok)
        | None -> None
      in
      let v5 = (* template_interpolation_end *) token env v5 in
      [ Right3 (v1, v3, v5) ]

and map_binary_operation (env : env) (x : CST.binary_operation) =
  match x with
  | `Expr_term_choice_STAR_expr_term (v1, v2, v3) ->
      let v1 = map_expr_term env v1 in
      let v2 =
        match v2 with
        | `STAR tok -> (* "*" *) (G.Mult, token env tok)
        | `SLASH tok -> (* "/" *) (G.Div, token env tok)
        | `PERC tok -> (* "%" *) (G.Mod, token env tok)
      in
      let v3 = map_expr_term env v3 in
      (v1, v2, v3)
  | `Expr_term_choice_PLUS_expr_term (v1, v2, v3) ->
      let v1 = map_expr_term env v1 in
      let v2 =
        match v2 with
        | `PLUS tok -> (* "+" *) (G.Plus, token env tok)
        | `DASH tok -> (* "-" *) (G.Minus, token env tok)
      in
      let v3 = map_expr_term env v3 in
      (v1, v2, v3)
  | `Expr_term_choice_GT_expr_term (v1, v2, v3) ->
      let v1 = map_expr_term env v1 in
      let v2 =
        match v2 with
        | `GT tok -> (* ">" *) (G.Gt, token env tok)
        | `GTEQ tok -> (* ">=" *) (G.GtE, token env tok)
        | `LT tok -> (* "<" *) (G.Lt, token env tok)
        | `LTEQ tok -> (* "<=" *) (G.LtE, token env tok)
      in
      let v3 = map_expr_term env v3 in
      (v1, v2, v3)
  | `Expr_term_choice_EQEQ_expr_term (v1, v2, v3) ->
      let v1 = map_expr_term env v1 in
      let v2 =
        match v2 with
        | `EQEQ tok -> (* "==" *) (G.Eq, token env tok)
        | `BANGEQ tok -> (* "!=" *) (G.NotEq, token env tok)
      in
      let v3 = map_expr_term env v3 in
      (v1, v2, v3)
  | `Expr_term_choice_AMPAMP_expr_term (v1, v2, v3) ->
      let v1 = map_expr_term env v1 in
      let v2 =
        match v2 with
        | `AMPAMP tok -> (* "&&" *) (G.And, token env tok)
      in
      let v3 = map_expr_term env v3 in
      (v1, v2, v3)
  | `Expr_term_choice_BARBAR_expr_term (v1, v2, v3) ->
      let v1 = map_expr_term env v1 in
      let v2 =
        match v2 with
        | `BARBAR tok -> (* "||" *) (G.Or, token env tok)
      in
      let v3 = map_expr_term env v3 in
      (v1, v2, v3)

and map_collection_value (env : env) (x : CST.collection_value) : expr =
  match x with
  | `Tuple (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_tuple_elems env x
        | None -> []
      in
      let v3 = (* "]" *) token env v3 in
      G.Container (G.Tuple, (v1, v2, v3)) |> G.e
  | `Obj x -> map_object_ env x

and map_expr_term (env : env) (x : CST.expr_term) : expr =
  match x with
  | `Choice_lit_value x -> (
      match x with
      | `Lit_value x -> G.L (map_literal_value env x) |> G.e
      | `Temp_expr x -> map_template_expr env x
      | `Coll_value x -> map_collection_value env x
      | `Var_expr tok ->
          (* identifier *)
          let id = map_identifier env tok in
          G.N (H2.name_of_id id) |> G.e
      | `Func_call (v1, v2, v3, v4) ->
          let v1 = (* identifier *) map_identifier env v1 in
          let v2 = (* "(" *) token env v2 in
          let v3 =
            match v3 with
            | Some x -> map_function_arguments env x
            | None -> []
          in
          let v4 = (* ")" *) token env v4 in
          let n = G.N (H2.name_of_id v1) |> G.e in
          G.Call (n, (v2, v3, v4)) |> G.e
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
          v2)
  | `Semg_ellips tok -> G.Ellipsis ((* "..." *) token env tok) |> G.e
  | `Deep_ellips (v1, v2, v3) ->
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "...>" *) token env v3 in
      G.DeepEllipsis (v1, v2, v3) |> G.e

and map_expression (env : env) (x : CST.expression) : expr =
  match x with
  | `Expr_term x -> map_expr_term env x
  | `Cond (v1, v2, v3, v4, v5) ->
      let v1 = map_expression env v1 in
      let _v2 = (* "?" *) token env v2 in
      let v3 = map_expression env v3 in
      let _v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      G.Conditional (v1, v3, v5) |> G.e

and map_for_cond (env : env) ((v1, v2) : CST.for_cond) : G.for_or_if_comp =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_expression env v2 in
  G.CompIf (v1, v2)

and map_for_expr (env : env) (x : CST.for_expr) =
  match x with
  | `For_tuple_expr (v1, v2, v3, v4, v5) ->
      let v1 = (* "[" *) token env v1 in
      let tfor, ids, tin, e, _tcolon = map_for_intro env v2 in
      let v3 = map_expression env v3 in
      let v4 =
        match v4 with
        | Some x -> [ map_for_cond env x ]
        | None -> []
      in
      let v5 = (* "]" *) token env v5 in
      let pat = pattern_of_ids ids in
      let compfor = G.CompFor (tfor, pat, tin, e) in
      let xs = compfor :: v4 in
      G.Comprehension (G.Tuple, (v1, (v3, xs), v5)) |> G.e
  | `For_obj_expr (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = (* "{" *) token env v1 in
      let tfor, ids, tin, e, _tcolon = map_for_intro env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "=>" *) token env v4 in
      let v5 = map_expression env v5 in
      (* ??? *)
      let _v6TODO =
        match v6 with
        | Some tok -> (* ellipsis *) Some (token env tok)
        | None -> None
      in
      let v7 =
        match v7 with
        | Some x -> [ map_for_cond env x ]
        | None -> []
      in
      let v8 = (* "}" *) token env v8 in
      let pat = pattern_of_ids ids in
      let compfor = G.CompFor (tfor, pat, tin, e) in
      let xs = compfor :: v7 in
      let ekeyval = G.keyval v3 v4 v5 in
      G.Comprehension (G.Dict, (v1, (ekeyval, xs), v8)) |> G.e

and map_for_intro (env : env) ((v1, v2, v3, v4, v5, v6) : CST.for_intro) =
  let v1 = (* "for" *) token env v1 in
  let v2 = (* identifier *) map_identifier env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = (* identifier *) map_identifier env v2 in
        [ v2 ]
    | None -> []
  in
  let v4 = (* "in" *) token env v4 in
  let v5 = map_expression env v5 in
  let v6 = (* ":" *) token env v6 in
  (v1, v2 :: v3, v4, v5, v6)

and map_function_arguments (env : env) ((v1, v2, v3) : CST.function_arguments) :
    G.argument list =
  let v1 = map_expression env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_expression env v2 in
        G.Arg v2)
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
            [ G.Arg (G.Ellipsis t |> G.e) ])
    | None -> []
  in
  [ G.Arg v1 ] @ v2 @ v3

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
      let idx = G.L (parse_number_literal v2) |> G.e in
      fun e -> G.ArrayAccess (e, (v1, idx, v1)) |> G.e

and map_object_ (env : env) ((v1, v2, v3) : CST.object_) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_object_elems env x
    | None -> []
  in
  let v3 = (* "}" *) token env v3 in
  G.Record (v1, v2, v3) |> G.e

and map_object_elem (env : env) (x : CST.object_elem) : G.field =
  match x with
  | `Exp_choice_EQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `EQ tok -> (* "=" *) Left (token env tok)
        | `COLON tok -> (* ":" *) Right (token env tok)
      in
      let v3 = map_expression env v3 in
      let n_or_dyn =
        match v1.e with
        | N n -> G.EN n
        | _ -> G.EDynamic v1
      in
      let ent = { G.name = n_or_dyn; attrs = []; tparams = None } in
      let vdef = { G.vinit = Some v3; vtype = None; vtok = G.no_sc } in
      let def =
        match v2 with
        | Left _teq -> G.VarDef vdef
        | Right _tcolon -> G.FieldDefColon vdef
      in
      (ent, def) |> G.fld
  | `Semg_ellips v1 ->
      let v1 = token env v1 in
      G.field_ellipsis v1

and map_object_elems (env : env) ((v1, v2, v3) : CST.object_elems) =
  let v1 = map_object_elem env v1 in
  let v2 =
    List_.map
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
    match v3 with
    | Some tok -> (* "," *) Some (token env tok)
    | None -> None
  in
  v1 :: v2

and map_operation (env : env) (x : CST.operation) : expr =
  match x with
  | `Un_oper (v1, v2) ->
      let op, t =
        match v1 with
        | `DASH tok -> (* "-" *) (G.Minus, token env tok)
        | `BANG tok -> (* "!" *) (G.Not, token env tok)
      in
      let v2 = map_expr_term env v2 in
      G.opcall (op, t) [ v2 ]
  | `Bin_oper x ->
      let a, (op, t), c = map_binary_operation env x in
      G.opcall (op, t) [ a; c ]

and map_splat (env : env) (x : CST.splat) =
  match x with
  | `Attr_splat (v1, v2) ->
      let v1 = (* ".*" *) token env v1 in
      let f1 e =
        let access = G.FDynamic (G.IdSpecial (G.HashSplat, v1) |> G.e) in
        G.DotAccess (e, v1, access) |> G.e
      in
      let v2 = List_.map (map_anon_choice_get_attr_7bbf24f env) v2 in
      fun e -> v2 |> List.fold_left (fun acc f -> f acc) (f1 e)
  | `Full_splat (v1, v2) ->
      let v1 = (* "[*]" *) token env v1 in
      let f1 e =
        let access = G.IdSpecial (G.HashSplat, v1) |> G.e in
        G.ArrayAccess (e, (v1, access, v1)) |> G.e
      in
      let v2 = List_.map (map_anon_choice_get_attr_7bbf24f env) v2 in
      fun e -> v2 |> List.fold_left (fun acc f -> f acc) (f1 e)

and map_template_expr (env : env) (x : CST.template_expr) =
  match x with
  | `Quoted_temp (v1, v2, v3) ->
      let v1 = (* quoted_template_start *) token env v1 in
      let v2 =
        match v2 with
        | Some xs -> List.concat_map (map_anon_choice_temp_lit_c764a73 env) xs
        | None -> []
      in
      let v3 = (* quoted_template_end *) token env v3 in
      G.interpolated (v1, v2, v3)
  | `Here_temp (v1, v2, v3, v4) ->
      let v1 = map_heredoc_start env v1 in
      let v2 = (* heredoc_identifier *) token env v2 in
      let v3 =
        match v3 with
        | Some xs -> List.concat_map (map_anon_choice_temp_lit_c764a73 env) xs
        | None -> []
      in
      let v4 = (* heredoc_identifier *) token env v4 in
      let t1 = Tok.combine_toks v1 [ v2 ] in
      G.interpolated (t1, v3, v4)

and map_tuple_elems (env : env) ((v1, v2, v3) : CST.tuple_elems) : expr list =
  let v1 = map_expression env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_expression env v2 in
        v2)
      v2
  in
  let _v3 =
    match v3 with
    | Some tok -> (* "," *) Some (token env tok)
    | None -> None
  in
  v1 :: v2

let map_attribute (env : env) ((v1, v2, v3) : CST.attribute) : argument =
  let v1 = map_identifier env v1 in
  let teq = (* "=" *) token env v2 in
  let v3 = map_expression env v3 in
  (v1, teq, v3)

let map_block_type env v1 : block_type wrap =
  let s, t = map_identifier env v1 in
  (block_type_of_string s, t)

let rec map_block (env : env) ((v1, v2, v3, v4, v5) : CST.block) : block =
  let btype = (* identifier *) map_block_type env v1 in
  let blabels =
    List_.map
      (fun x ->
        match x with
        | `Str_lit x ->
            let _, x, _ = map_string_lit env x in
            LblStr x
        | `Id tok ->
            let id = (* identifier *) map_identifier env tok in
            LblId id)
      v2
  in
  let lb = (* "{" *) token env v3 in
  let body =
    match v4 with
    | Some x -> map_body env x
    | None -> []
  in
  let rb = (* "}" *) token env v5 in
  { btype; blabels; bbody = (lb, body, rb) }

and map_body (env : env) (xs : CST.body) : block_body_element list =
  List_.map
    (fun x ->
      match x with
      | `Attr x ->
          let arg = map_attribute env x in
          Argument arg
      | `Blk x ->
          let blk = map_block env x in
          Block blk
      | `Semg_ellips tok ->
          let t = (* "..." *) token env tok in
          BlockEllipsis t)
    xs

and map_body_top (env : env) (xs : CST.body) : config =
  (* in theory we should forbid the case for `Attr here *)
  map_body env xs

let map_config_file (env : env) (x : CST.config_file) : any =
  match x with
  | `Opt_choice_body opt -> (
      match opt with
      | Some x -> (
          match x with
          | `Body x ->
              let top = map_body_top env x in
              Pr top
          | `Obj x ->
              let x = map_object_ env x in
              E x)
      | None -> Pr [])
  | `Semg_exp (v1, v2) ->
      let _v1 = (* "__SEMGREP_EXPRESSION" *) token env v1 in
      let v2 = map_expression env v2 in
      E v2

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_hcl.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      match map_config_file env cst with
      | Pr xs -> xs
      | _ -> failwith "not a program")

let parse_expression_or_source_file str =
  let res = Tree_sitter_hcl.Parse.string str in
  match res.errors with
  | [] -> res
  | _ ->
      (* classic semgrep parsing hack (see semgrep-hcl/grammar.js) *)
      let expr_str = "__SEMGREP_EXPRESSION " ^ str in
      Tree_sitter_hcl.Parse.string expr_str

let parse_pattern str =
  H.wrap_parser
    (fun () -> parse_expression_or_source_file str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      map_config_file env cst)
