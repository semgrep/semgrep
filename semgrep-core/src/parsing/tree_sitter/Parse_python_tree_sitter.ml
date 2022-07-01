(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
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
let fb = AST_generic.fake_bracket

(* AST builders helpers
 * less: could be moved in AST_Python.ml to factorize things with
 * parser_python.mly
 *)

let name_of_id id = Name (id, no_ctx, ref NotResolved)

let single_or_tuple e xs =
  match xs with
  | [] -> e
  | _ -> Tuple (CompList (fb (e :: xs)), no_ctx)

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
let _complicated (env : env) _ = failwith "not implemented"

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
  Common.map (token env (* "." *)) xs

let map_dotted_name (env : env) ((v1, v2) : CST.dotted_name) : dotted_name =
  let v1 = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v1 in
  let v2 =
    Common.map
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
    : (name, expr) Common.either =
  match x with
  | `Id tok ->
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok in
      Left id
  | `Choice_print x ->
      let id = map_keyword_identifier env x in
      Left id
  | `Subs x -> Right (map_subscript env x)
  | `Attr x -> Right (map_attribute env x)

and map_anon_choice_pair_002ffed (env : env) (x : CST.anon_choice_pair_002ffed)
    : dictorset_elt =
  match x with
  | `Pair x ->
      let k, v = map_pair env x in
      KeyVal (k, v)
  | `Dict_splat x ->
      let _tpow, e = map_dictionary_splat env x in
      PowInline e

and map_anon_choice_type_03d361f (env : env) (x : CST.anon_choice_type_03d361f)
    : expr =
  match x with
  | `Exp x -> map_type_ env x
  | `Yield x -> map_yield env x
  | `List_splat x ->
      let _tstar, e = map_list_splat env x in
      ExprStar e
  | `Paren_list_splat x -> map_parenthesized_list_splat env x

and map_expr_opt env v =
  match v with
  | Some x -> Some (map_expression env x)
  | None -> None

and map_anon_choice_type_a577897 (env : env) (x : CST.anon_choice_type_a577897)
    : slice =
  match x with
  | `Exp x ->
      let e = map_type_ env x in
      Index e
  | `Slice (v1, v2, v3, v4) ->
      let e1 = map_expr_opt env v1 in
      let _tcolon1 = (* ":" *) token env v2 in
      let e2 = map_expr_opt env v3 in
      let e3 =
        match v4 with
        | Some (v1, v2) ->
            let _tcolon2 = (* ":" *) token env v1 in
            let e3 = map_expr_opt env v2 in
            e2
        | None -> None
      in
      Slice (e1, e2, e3)

and map_anon_choice_type_aad5b2d (env : env) (x : CST.anon_choice_type_aad5b2d)
    : argument =
  match x with
  | `Exp x ->
      let e = map_type_ env x in
      Arg e
  | `List_splat x ->
      let tstar, e = map_list_splat env x in
      ArgStar (tstar, e)
  | `Dict_splat x ->
      let tpow, e = map_dictionary_splat env x in
      ArgPow (tpow, e)
  | `Paren_list_splat x ->
      let e = map_parenthesized_list_splat env x in
      Arg e
  | `Kw_arg (v1, v2, v3) ->
      let id =
        match v1 with
        | `Id tok ->
            (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok
        | `Choice_print x -> map_keyword_identifier env x
      in
      let _teq = (* "=" *) token env v2 in
      let e = map_type_ env v3 in
      ArgKwd (id, e)

(* not in original grammar, but help to factorize boilerplate code *)
and map_trailing_comma env v =
  match v with
  | Some tok -> Some ((* "," *) token env tok)
  | None -> None

and map_argument_list (env : env) ((v1, v2, v3, v4) : CST.argument_list) =
  let lp = (* "(" *) token env v1 in
  let args =
    match v2 with
    | Some (v1, v2) ->
        let arg = map_anon_choice_type_aad5b2d env v1 in
        let args =
          Common.map
            (fun (v1, v2) ->
              let _tcomma = (* "," *) token env v1 in
              let arg = map_anon_choice_type_aad5b2d env v2 in
              arg)
            v2
        in
        arg :: args
    | None -> []
  in
  let _ = map_trailing_comma env v3 in
  let rp = (* ")" *) token env v4 in
  (lp, args, rp)

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
      BinOp (v1, (Add, v2), v3)
  | `Prim_exp_DASH_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      BinOp (v1, (Sub, v2), v3)
  | `Prim_exp_STAR_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      BinOp (v1, (Mult, v2), v3)
  | `Prim_exp_AT_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "@" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      BinOp (v1, (MatMult, v2), v3)
  | `Prim_exp_SLASH_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      BinOp (v1, (Div, v2), v3)
  | `Prim_exp_PERC_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      BinOp (v1, (Mod, v2), v3)
  | `Prim_exp_SLASHSLASH_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "//" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      BinOp (v1, (FloorDiv, v2), v3)
  | `Prim_exp_STARSTAR_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "**" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      BinOp (v1, (Pow, v2), v3)
  | `Prim_exp_BAR_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      BinOp (v1, (BitOr, v2), v3)
  | `Prim_exp_AMP_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      BinOp (v1, (BitAnd, v2), v3)
  | `Prim_exp_HAT_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      BinOp (v1, (BitXor, v2), v3)
  | `Prim_exp_LTLT_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      BinOp (v1, (LShift, v2), v3)
  | `Prim_exp_GTGT_prim_exp (v1, v2, v3) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      BinOp (v1, (RShift, v2), v3)

and map_boolean_operator (env : env) (x : CST.boolean_operator) =
  match x with
  | `Exp_and_exp (v1, v2, v3) ->
      let v1 = map_type_ env v1 in
      let v2 = (* "and" *) token env v2 in
      let v3 = map_type_ env v3 in
      BoolOp ((And, v2), [ v1; v3 ])
  | `Exp_or_exp (v1, v2, v3) ->
      let v1 = map_type_ env v1 in
      let v2 = (* "or" *) token env v2 in
      let v3 = map_type_ env v3 in
      BoolOp ((Or, v2), [ v1; v3 ])

and map_collection_elements (env : env) ((v1, v2, v3) : CST.collection_elements)
    : expr list =
  let v1 = map_anon_choice_type_03d361f env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_type_03d361f env v2 in
        v2)
      v2
  in
  let _ = map_trailing_comma env v3 in
  v1 :: v2

and map_comprehension_clauses (env : env) ((v1, v2) : CST.comprehension_clauses)
    : for_if list =
  let v1 = map_for_in_clause env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `For_in_clause x -> map_for_in_clause env x
        | `If_clause x -> map_if_clause env x)
      v2
  in
  v1 :: v2

and map_dictionary_splat (env : env) ((v1, v2) : CST.dictionary_splat) =
  let v1 = (* "**" *) token env v1 in
  let v2 = map_type_ env v2 in
  (v1, v2)

and map_dictionary_splat_pattern (env : env)
    ((v1, v2) : CST.dictionary_splat_pattern) =
  let v1 = (* "**" *) token env v1 in
  let v2 = map_anon_choice_id_b80cb38 env v2 in
  (v1, v2)

and map_expression (env : env) (x : CST.expression) : expr =
  match x with
  | `Comp_op (v1, v2) ->
      let e = map_primary_expression env v1 in
      let xs =
        Common.map
          (fun (v1, v2) ->
            let v1 =
              match v1 with
              | `LT tok -> (Lt, (* "<" *) token env tok)
              | `LTEQ tok -> (LtE, (* "<=" *) token env tok)
              | `EQEQ tok -> (Eq, (* "==" *) token env tok)
              | `BANGEQ tok -> (NotEq, (* "!=" *) token env tok)
              | `GTEQ tok -> (GtE, (* ">=" *) token env tok)
              | `GT tok -> (Gt, (* ">" *) token env tok)
              (* <> equivalent to !=? *)
              | `LTGT tok -> (NotEq, (* "<>" *) token env tok)
              | `In tok -> (In, (* "in" *) token env tok)
              | `Is tok -> (Is, (* "is" *) token env tok)
              (* TODO? PI.combine_infos? *)
              | `Not_in (v1, v2) ->
                  let v1 = (* "not" *) token env v1 in
                  let _v2 = (* "in" *) token env v2 in
                  (NotIn, v1)
              | `Is_not (v1, v2) ->
                  let v1 = (* "is" *) token env v1 in
                  let _v2 = (* "not" *) token env v2 in
                  (IsNot, v1)
            in
            let v2 = map_primary_expression env v2 in
            (v1, v2))
          v2
      in
      Compare (e, xs |> Common.map fst, xs |> Common.map snd)
  | `Not_op (v1, v2) ->
      let v1 = (* "not" *) token env v1 in
      let v2 = map_type_ env v2 in
      UnaryOp ((Not, v1), v2)
  | `Bool_op x -> map_boolean_operator env x
  | `Await (v1, v2) ->
      let v1 = (* "await" *) token env v1 in
      let v2 = map_type_ env v2 in
      Await (v1, v2)
  | `Lambda (v1, v2, v3, v4) ->
      let tlambda = (* "lambda" *) token env v1 in
      let params =
        match v2 with
        | Some x -> map_lambda_parameters env x
        | None -> []
      in
      let tcolon = (* ":" *) token env v3 in
      let body = map_type_ env v4 in
      Lambda (tlambda, params, tcolon, body)
  | `Prim_exp x -> map_primary_expression env x
  | `Cond_exp (v1, v2, v3, v4, v5) ->
      let v1 = map_type_ env v1 in
      let _v2 = (* "if" *) token env v2 in
      let v3 = map_type_ env v3 in
      let _v4 = (* "else" *) token env v4 in
      let v5 = map_type_ env v5 in
      IfExp (v3, v1, v5)
  | `Named_exp (v1, v2, v3) ->
      (* TODO? pfff allows any expr on lhs *)
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v1 in
      let teq = (* ":=" *) token env v2 in
      let e = map_type_ env v3 in
      NamedExpr (name_of_id id, teq, e)

and map_expression_list (env : env) ((v1, v2) : CST.expression_list) : expr list
    =
  let v1 = map_type_ env v1 in
  let v2 =
    match v2 with
    | `COMMA tok ->
        let _trailing = (* "," *) token env tok in
        []
    | `Rep1_COMMA_exp_opt_COMMA (v1, v2) ->
        let v1 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_type_ env v2 in
              v2)
            v1
        in
        let _ = map_trailing_comma env v2 in
        v1
  in
  v1 :: v2

and map_expression_within_for_in_clause (env : env)
    (x : CST.expression_within_for_in_clause) : expr =
  match x with
  | `Exp x -> map_type_ env x
  | `Lambda_within_for_in_clause (v1, v2, v3, v4) ->
      let tlambda = (* "lambda" *) token env v1 in
      let params =
        match v2 with
        | Some x -> map_lambda_parameters env x
        | None -> []
      in
      let tcolon = (* ":" *) token env v3 in
      let body = map_expression_within_for_in_clause env v4 in
      Lambda (tlambda, params, tcolon, body)

and map_expressions (env : env) (x : CST.expressions) : expr =
  match x with
  | `Exp x -> map_type_ env x
  | `Exp_list x ->
      let xs = map_expression_list env x in
      Tuple (CompList (fb xs), no_ctx)

and map_expressions2 (env : env) (x : CST.expressions) : expr list =
  match x with
  | `Exp x -> [ map_type_ env x ]
  | `Exp_list x ->
      let xs = map_expression_list env x in
      xs

and map_for_in_clause (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.for_in_clause) : for_if =
  let _asyncTODO = map_async_opt env v1 in
  let _tfor = (* "for" *) token env v2 in
  let lhs = map_left_hand_side env v3 in
  let _tin = (* "in" *) token env v4 in
  let e = map_expression_within_for_in_clause env v5 in
  let xs =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_expression_within_for_in_clause env v2 in
        v2)
      v6
  in
  let _ = map_trailing_comma env v7 in
  let rhs = single_or_tuple e xs in
  CompFor (lhs, rhs)

(* not in original grammar, but useful to factorize code *)
and map_async_opt env v1 =
  match v1 with
  | Some tok -> Some ((* "async" *) token env tok)
  | None -> None

and map_format_expression (env : env) ((v1, v2, v3) : CST.format_expression) =
  let _lc = (* "{" *) token env v1 in
  let e = map_type_ env v2 in
  let _rc = (* "}" *) token env v3 in
  e

and map_format_specifier (env : env) ((v1, v2) : CST.format_specifier) :
    expr list =
  let _tcolon = (* ":" *) token env v1 in
  let xs =
    Common.map
      (fun x ->
        match x with
        | `LBRA tok ->
            let x = (* [^{}\n]+ *) str env tok in
            Str x
        | `Format_exp x ->
            let x = map_format_expression env x in
            x)
      v2
  in
  xs

and map_generator_expression (env : env)
    ((v1, v2, v3, v4) : CST.generator_expression) : expr =
  let lp = (* "(" *) token env v1 in
  let e = map_type_ env v2 in
  let forifs = map_comprehension_clauses env v3 in
  let rp = (* ")" *) token env v4 in
  Tuple (CompForIf (lp, (e, forifs), rp), no_ctx)

and map_if_clause (env : env) ((v1, v2) : CST.if_clause) : for_if =
  let _tif = (* "if" *) token env v1 in
  let e = map_type_ env v2 in
  CompIf e

and map_interpolation (env : env) ((v1, v2, v3, v4, v5) : CST.interpolation) =
  let lb = (* "{" *) token env v1 in
  let e = map_type_ env v2 in
  let _type_conv_opt =
    match v3 with
    | Some tok -> Some ((* pattern ![a-z] *) str env tok)
    | None -> None
  in
  let _format_opt =
    match v4 with
    | Some x -> Some (map_format_specifier env x)
    | None -> None
  in
  let rb = (* "}" *) token env v5 in
  (lb, e, rb)

and map_lambda_parameters (env : env) (x : CST.lambda_parameters) =
  map_parameters_ env x

and map_left_hand_side (env : env) (x : CST.left_hand_side) : pattern =
  match x with
  | `Pat x -> map_pattern env x
  | `Pat_list (v1, v2) ->
      let pat = map_pattern env v1 in
      let pats =
        match v2 with
        | `COMMA tok ->
            let _trailing = (* "," *) token env tok in
            []
        | `Rep1_COMMA_pat_opt_COMMA (v1, v2) ->
            let v1 =
              Common.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_pattern env v2 in
                  v2)
                v1
            in
            let _v2 = map_trailing_comma env v2 in
            v1
      in
      single_or_tuple pat pats

and map_list_splat (env : env) ((v1, v2) : CST.list_splat) =
  let v1 = (* "*" *) token env v1 in
  let v2 = map_type_ env v2 in
  (v1, v2)

and map_list_splat_pattern (env : env) ((v1, v2) : CST.list_splat_pattern) =
  let v1 = (* "*" *) token env v1 in
  let v2 = map_anon_choice_id_b80cb38 env v2 in
  (v1, v2)

and map_pair (env : env) ((v1, v2, v3) : CST.pair) =
  let key = map_type_ env v1 in
  let _v2 = (* ":" *) token env v2 in
  let val_ = map_type_ env v3 in
  (key, val_)

and map_parameter (env : env) (x : CST.parameter) : parameter =
  match x with
  | `Id tok ->
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok in
      ParamPattern (PatternName id, None)
  | `Typed_param (v1, v2, v3) ->
      let pat =
        match v1 with
        | `Id tok ->
            let id =
              (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok
            in
            PatternName id
        | `List_splat_pat x ->
            let tstar, either = map_list_splat_pattern env x in
            todo env (tstar, either)
        | `Dict_splat_pat x ->
            let tpow, either = map_dictionary_splat_pattern env x in
            todo env (tpow, either)
      in
      let _tcolon = (* ":" *) token env v2 in
      let ty = map_type_ env v3 in
      ParamPattern (pat, Some ty)
  | `Defa_param (v1, v2, v3) ->
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v1 in
      let _teq = (* "=" *) token env v2 in
      let e = map_type_ env v3 in
      ParamDefault ((id, None), e)
  | `Typed_defa_param (v1, v2, v3, v4, v5) ->
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v1 in
      let _tcolon = (* ":" *) token env v2 in
      let ty = map_type_ env v3 in
      let _teq = (* "=" *) token env v4 in
      let e = map_type_ env v5 in
      ParamDefault ((id, Some ty), e)
  | `List_splat_pat x ->
      let tstar, either = map_list_splat_pattern env x in
      todo env (tstar, either)
  | `Tuple_pat x ->
      let _lp, xs, _rp = map_tuple_pattern env x in
      todo env xs
  | `STAR tok ->
      let t = (* "*" *) token env tok in
      ParamSingleStar t
  | `Dict_splat_pat x ->
      let tpow, either = map_dictionary_splat_pattern env x in
      todo env (tpow, either)

and map_parameters_ (env : env) ((v1, v2, v3) : CST.parameters_) =
  let v1 = map_parameter env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_parameter env v2 in
        v2)
      v2
  in
  let _ = map_trailing_comma env v3 in
  v1 :: v2

(* TODO: I have no idea what are those, why those list splats
 * need parenthesis around.
 *)
and map_parenthesized_list_splat (env : env)
    ((v1, v2, v3) : CST.parenthesized_list_splat) : expr =
  let _lp = (* "(" *) token env v1 in
  let e =
    match v2 with
    | `Paren_list_splat x -> map_parenthesized_list_splat env x
    | `List_splat x ->
        let _tstar, e = map_list_splat env x in
        ExprStar e
  in
  let _rp = (* ")" *) token env v3 in
  e

and map_pattern (env : env) (x : CST.pattern) : pattern =
  match x with
  | `Id tok ->
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok in
      name_of_id id
  | `Choice_print x ->
      let id = map_keyword_identifier env x in
      name_of_id id
  | `Subs x -> map_subscript env x
  | `Attr x -> map_attribute env x
  | `List_splat_pat x ->
      let tstar, either = map_list_splat_pattern env x in
      todo env (tstar, either)
  | `Tuple_pat x ->
      let lp, xs, rp = map_tuple_pattern env x in
      Tuple (CompList (lp, xs, rp), no_ctx)
  | `List_pat (v1, v2, v3) ->
      let lb = (* "[" *) token env v1 in
      let xs =
        match v2 with
        | Some x -> map_patterns env x
        | None -> []
      in
      let rb = (* "]" *) token env v3 in
      List (CompList (lb, xs, rb), no_ctx)

and map_patterns (env : env) ((v1, v2, v3) : CST.patterns) : pattern list =
  let v1 = map_pattern env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_pattern env v2 in
        v2)
      v2
  in
  (* less? trailing comma important when on lhs? *)
  let _ = map_trailing_comma env v3 in
  v1 :: v2

and map_primary_expression (env : env) (x : CST.primary_expression) : expr =
  match x with
  | `Bin_op x -> map_binary_operator env x
  | `Id tok ->
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok in
      name_of_id id
  | `Choice_print x ->
      let id = map_keyword_identifier env x in
      name_of_id id
  | `Str x ->
      let x = map_string_ env x in
      todo env x
  | `Conc_str (v1, v2) ->
      let v1 = map_string_ env v1 in
      let v2 = Common.map (map_string_ env) v2 in
      todo env (v1, v2)
  | `Int tok ->
      let s, tk = (* integer *) str env tok in
      Num (Int (int_of_string_opt s, tk))
  | `Float tok ->
      let s, tk = (* float *) str env tok in
      Num (Float (float_of_string_opt s, tk))
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
      let op =
        match v1 with
        | `PLUS tok -> (UAdd, (* "+" *) token env tok)
        | `DASH tok -> (USub, (* "-" *) token env tok)
        | `TILDE tok -> (Invert, (* "~" *) token env tok)
      in
      let e = map_primary_expression env v2 in
      UnaryOp (op, e)
  | `Attr x -> map_attribute env x
  | `Subs x -> map_subscript env x
  | `Call (v1, v2) ->
      let f = map_primary_expression env v1 in
      let args =
        match v2 with
        | `Gene_exp x ->
            (* TODO: should transform in ArgComp, but we should
             * get rid of ArgComp anyway *)
            let x = map_generator_expression env x in
            fb [ Arg x ]
        | `Arg_list x -> map_argument_list env x
      in
      Call (f, args)
  | `List (v1, v2, v3) ->
      let lb = (* "[" *) token env v1 in
      let xs =
        match v2 with
        | Some x -> map_collection_elements env x
        | None -> []
      in
      let rb = (* "]" *) token env v3 in
      List (CompList (lb, xs, rb), no_ctx)
  | `List_comp (v1, v2, v3, v4) ->
      let lb = (* "[" *) token env v1 in
      let e = map_type_ env v2 in
      let for_ifs = map_comprehension_clauses env v3 in
      let rb = (* "]" *) token env v4 in
      List (CompForIf (lb, (e, for_ifs), rb), no_ctx)
  | `Dict (v1, v2, v3, v4) ->
      let l = (* "{" *) token env v1 in
      let xs =
        match v2 with
        | Some (v1, v2) ->
            let v1 = map_anon_choice_pair_002ffed env v1 in
            let v2 =
              Common.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_anon_choice_pair_002ffed env v2 in
                  v2)
                v2
            in
            v1 :: v2
        | None -> []
      in
      let _ = map_trailing_comma env v3 in
      let r = (* "}" *) token env v4 in
      DictOrSet (CompList (l, xs, r))
  | `Dict_comp (v1, v2, v3, v4) ->
      let l = (* "{" *) token env v1 in
      let k, v = map_pair env v2 in
      let el = KeyVal (k, v) in
      let for_ifs = map_comprehension_clauses env v3 in
      let r = (* "}" *) token env v4 in
      DictOrSet (CompForIf (l, (el, for_ifs), r))
  | `Set (v1, v2, v3) ->
      let l = (* "{" *) token env v1 in
      let xs = map_collection_elements env v2 in
      let r = (* "}" *) token env v3 in
      let ys = xs |> Common.map (fun e -> Key e) in
      DictOrSet (CompList (l, ys, r))
  | `Set_comp (v1, v2, v3, v4) ->
      let l = (* "{" *) token env v1 in
      let e = map_type_ env v2 in
      let el = Key e in
      let for_ifs = map_comprehension_clauses env v3 in
      let r = (* "}" *) token env v4 in
      DictOrSet (CompForIf (l, (el, for_ifs), r))
  | `Tuple (v1, v2, v3) ->
      let l = (* "(" *) token env v1 in
      let xs =
        match v2 with
        | Some x -> map_collection_elements env x
        | None -> []
      in
      let r = (* ")" *) token env v3 in
      Tuple (CompList (l, xs, r), no_ctx)
  | `Paren_exp (v1, v2, v3) ->
      let _lp = (* "(" *) token env v1 in
      let e =
        match v2 with
        | `Exp x -> map_type_ env x
        | `Yield x -> map_yield env x
      in
      let _rp = (* ")" *) token env v3 in
      (* TODO? ParenExpr? *)
      e
  | `Gene_exp x ->
      let x = map_generator_expression env x in
      x
  | `Ellips tok ->
      let t = (* "..." *) token env tok in
      Ellipsis t

and map_string_ (env : env) ((v1, v2, v3) : CST.string_) :
    interpolated list bracket =
  let str_start = (* string_start *) token env v1 in
  let xs =
    Common.map
      (fun x ->
        match x with
        | `Interp x ->
            let _lb, e, _rb = map_interpolation env x in
            e
        | `Esc_interp x ->
            let s = map_escape_interpolation env x in
            Str s
        | `Esc_seq tok ->
            let s = (* escape_sequence *) str env tok in
            Str s
        | `Not_esc_seq tok ->
            let s = (* "\\" *) str env tok in
            Str s
        | `Str_content tok ->
            let s = (* string_content *) str env tok in
            Str s)
      v2
  in
  let str_end = (* string_end *) token env v3 in
  (str_start, xs, str_end)

and map_subscript (env : env) ((v1, v2, v3, v4, v5, v6) : CST.subscript) =
  let e = map_primary_expression env v1 in
  let l = (* "[" *) token env v2 in
  let slice = map_anon_choice_type_a577897 env v3 in
  let slices =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_type_a577897 env v2 in
        v2)
      v4
  in
  let _ = map_trailing_comma env v5 in
  let r = (* "]" *) token env v6 in
  Subscript (e, (l, slice :: slices, r), no_ctx)

and map_tuple_pattern (env : env) ((v1, v2, v3) : CST.tuple_pattern) :
    pattern list bracket =
  let lp = (* "(" *) token env v1 in
  let xs =
    match v2 with
    | Some x -> map_patterns env x
    | None -> []
  in
  let rp = (* ")" *) token env v3 in
  (lp, xs, rp)

and map_type_ (env : env) (x : CST.type_) : type_ = map_expression env x

and map_yield (env : env) ((v1, v2) : CST.yield) : expr =
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

let map_relative_import (env : env) ((v1, v2) : CST.relative_import) :
    module_name =
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
  | `Exp x ->
      let e = map_type_ env x in
      todo env e
  | `Exp_list x ->
      let xs = map_expression_list env x in
      let e = Tuple (CompList (fb xs), no_ctx) in
      todo env e
  | `Assign x -> map_assignment env x
  | `Augm_assign x -> map_augmented_assignment env x
  | `Yield x -> map_yield env x

let map_decorator (env : env) ((v1, v2, v3) : CST.decorator) =
  let tat = (* "@" *) token env v1 in
  let e = map_primary_expression env v2 in
  let _newline = (* newline *) token env v3 in
  todo env (v1, v2, v3)

(* python2? *)
let map_chevron (env : env) ((v1, v2) : CST.chevron) =
  let v1 = (* ">>" *) token env v1 in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

let map_parameters (env : env) ((v1, v2, v3) : CST.parameters) : parameters =
  let _l = (* "(" *) token env v1 in
  let xs =
    match v2 with
    | Some x -> map_lambda_parameters env x
    | None -> []
  in
  let _r = (* ")" *) token env v3 in
  xs

let map_anon_choice_dotted_name_c5c573a (env : env)
    (x : CST.anon_choice_dotted_name_c5c573a) =
  match x with
  | `Dotted_name x ->
      let m = map_dotted_name env x in
      (m, None)
  | `Alia_import (v1, v2, v3) ->
      let m = map_dotted_name env v1 in
      let _tas = (* "as" *) token env v2 in
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v3 in
      (m, Some id)

let map_with_clause (env : env) (x : CST.with_clause) : with_clause =
  match x with
  | `With_item_rep_COMMA_with_item (v1, v2) ->
      let v1 = map_with_item env v1 in
      let v2 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_with_item env v2 in
            v2)
          v2
      in
      todo env (v1, v2)
  | `LPAR_with_item_rep_COMMA_with_item_RPAR (v1, v2, v3, v4) ->
      let _l = (* "(" *) token env v1 in
      let v2 = map_with_item env v2 in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_with_item env v2 in
            v2)
          v3
      in
      let _r = (* ")" *) token env v4 in
      todo env (v2, v3)

let map_expression_statement (env : env) (x : CST.expression_statement) : expr =
  match x with
  | `Exp x -> map_type_ env x
  | `Exp_rep_COMMA_exp_opt_COMMA (v1, v2, v3) ->
      let v1 = map_type_ env v1 in
      let v2 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_type_ env v2 in
            v2)
          v2
      in
      let _ = map_trailing_comma env v3 in
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
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_type_ env v2 in
            v2)
          v3
      in
      let v4 = map_trailing_comma env v4 in
      todo env (v1, v2, v3, v4)
  | `Print_exp_rep_COMMA_exp_opt_COMMA (v1, v2, v3, v4) ->
      let v1 = (* "print" *) token env v1 in
      let v2 = map_type_ env v2 in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_type_ env v2 in
            v2)
          v3
      in
      let _v4 = map_trailing_comma env v4 in
      todo env (v1, v2, v3, v4)

let map_import_list (env : env) ((v1, v2, v3) : CST.import_list) =
  let v1 = map_anon_choice_dotted_name_c5c573a env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_dotted_name_c5c573a env v2 in
        v2)
      v2
  in
  let v3 = map_trailing_comma env v3 in
  v1 :: v2

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
            let _v1 = (* "(" *) token env v1 in
            let v2 = map_import_list env v2 in
            let _v3 = (* ")" *) token env v3 in
            v2
      in
      todo env (v1, v2, v3, v4)
  | `Import_stmt (v1, v2) ->
      let v1 = (* "import" *) token env v1 in
      let v2 = map_import_list env v2 in
      todo env (v1, v2)
  | `Import_from_stmt (v1, v2, v3, v4) ->
      let tfrom = (* "from" *) token env v1 in
      let path =
        match v2 with
        | `Rela_import x -> map_relative_import env x
        | `Dotted_name x -> (map_dotted_name env x, None)
      in
      let _timport = (* "import" *) token env v3 in
      let xs =
        match v4 with
        | `Wild_import tok ->
            let id = (* "*" *) str env tok in
            [ (id, None) ]
        | `Import_list x ->
            (* TODO: in pfff this can be a name * name option *)
            let xs = map_import_list env x in
            todo env xs
        | `LPAR_import_list_RPAR (v1, v2, v3) ->
            let _v1 = (* "(" *) token env v1 in
            let xs = map_import_list env v2 in
            let _v3 = (* ")" *) token env v3 in
            todo env xs
      in
      ImportFrom (tfrom, path, xs)
  | `Print_stmt x -> map_print_statement env x
  | `Assert_stmt (v1, v2, v3) ->
      let tassert = (* "assert" *) token env v1 in
      let test = map_type_ env v2 in
      let _extraTODO =
        Common.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_type_ env v2 in
            todo env (v1, v2))
          v3
      in
      Assert (tassert, test, None)
  | `Exp_stmt x ->
      let e = map_expression_statement env x in
      ExprStmt e
  | `Ret_stmt (v1, v2) ->
      let tret = (* "return" *) token env v1 in
      let eopt =
        match v2 with
        | Some x -> Some (map_expressions env x)
        | None -> None
      in
      Return (tret, eopt)
  | `Delete_stmt (v1, v2) ->
      let tdel = (* "del" *) token env v1 in
      let xs = map_expressions2 env v2 in
      Delete (tdel, xs)
  | `Raise_stmt (v1, v2, v3) ->
      let traise = (* "raise" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_expressions env x
        | None -> todo env ()
      in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let _tfrom = (* "from" *) token env v1 in
            let v2 = map_type_ env v2 in
            Some v2
        | None -> None
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
      let tglobal = (* "global" *) token env v1 in
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v2 in
      let ids =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 =
              (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v2
            in
            v2)
          v3
      in
      Global (tglobal, id :: ids)
  | `Nonl_stmt (v1, v2, v3) ->
      let tnonlocal = (* "nonlocal" *) token env v1 in
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v2 in
      let ids =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 =
              (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v2
            in
            v2)
          v3
      in
      NonLocal (tnonlocal, id :: ids)
  | `Exec_stmt (v1, v2, v3) ->
      let v1 = (* "exec" *) token env v1 in
      let v2 = map_string_ env v2 in
      let v3 =
        match v3 with
        | Some (v1, v2, v3) ->
            let v1 = (* "in" *) token env v1 in
            let v2 = map_type_ env v2 in
            let v3 =
              Common.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_type_ env v2 in
                  v2)
                v3
            in
            Some (v1, v2 :: v3)
        | None -> None
      in
      todo env (v1, v2, v3)

let map_simple_statements (env : env) ((v1, v2, v3, v4) : CST.simple_statements)
    : stmt list =
  let v1 = map_simple_statement env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* ";" *) token env v1 in
        let v2 = map_simple_statement env v2 in
        v2)
      v2
  in
  let _trailing_semicolon =
    match v3 with
    | Some tok -> Some ((* ";" *) token env tok)
    | None -> None
  in
  let _v4 = (* newline *) token env v4 in
  v1 :: v2

let rec map_block (env : env) ((v1, v2) : CST.block) =
  let v1 = map_module_ env v1 in
  let _v2 = (* dedent *) token env v2 in
  v1

and map_class_definition (env : env)
    ((v1, v2, v3, v4, v5) : CST.class_definition) : class_definition =
  let tclass = (* "class" *) token env v1 in
  let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v2 in
  let parents =
    match v3 with
    | Some x ->
        let _l, xs, _r = map_argument_list env x in
        xs
    | None -> []
  in
  let _colon = (* ":" *) token env v4 in
  let body = map_suite env v5 in
  (tclass, id, parents, body, [])

(* not in original grammar, but help to factorize boilerplate code *)
and map_or_else_as_list env v =
  match v with
  | Some x -> map_else_clause env x
  | None -> []

and map_compound_statement (env : env) (x : CST.compound_statement) : stmt =
  match x with
  | `If_stmt (v1, v2, v3, v4, v5, v6) ->
      let tif = (* "if" *) token env v1 in
      let cond = map_type_ env v2 in
      let _tcolon = (* ":" *) token env v3 in
      let then_ = map_suite env v4 in
      let elseifs = Common.map (map_elif_clause env) v5 in
      let else_ =
        match v6 with
        | Some x -> map_else_clause env x
        | None -> todo env ()
      in
      let orelse = todo env (elseifs, else_) in
      If (tif, cond, then_, orelse)
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let _asyncTODO = map_async_opt env v1 in
      let tfor = (* "for" *) token env v2 in
      let lhs = map_left_hand_side env v3 in
      let tin = (* "in" *) token env v4 in
      let rhs = map_expressions env v5 in
      let _tcolon = (* ":" *) token env v6 in
      let body = map_suite env v7 in
      let orelse = map_or_else_as_list env v8 in
      For (tfor, lhs, tin, rhs, body, orelse)
  | `While_stmt (v1, v2, v3, v4, v5) ->
      let twhile = (* "while" *) token env v1 in
      let cond = map_type_ env v2 in
      let _tcolon = (* ":" *) token env v3 in
      let body = map_suite env v4 in
      let orelse = map_or_else_as_list env v5 in
      While (twhile, cond, body, orelse)
  | `Try_stmt (v1, v2, v3, v4) ->
      let ttry = (* "try" *) token env v1 in
      let _tcolon = (* ":" *) token env v2 in
      let body = map_suite env v3 in
      let res =
        match v4 with
        | `Rep1_except_clause_opt_else_clause_opt_fina_clause (v1, v2, v3) ->
            let excepts = Common.map (map_except_clause env) v1 in
            let orelse = map_or_else_as_list env v2 in
            let _finallyTODO =
              match v3 with
              | Some x ->
                  let tfinal, finalbody = map_finally_clause env x in
                  todo env (tfinal, finalbody)
              | None -> None
            in
            TryExcept (ttry, body, excepts, orelse)
        | `Fina_clause x ->
            let tfinal, finalbody = map_finally_clause env x in
            TryFinally (ttry, body, tfinal, finalbody)
      in
      res
  | `With_stmt (v1, v2, v3, v4, v5) ->
      let _asyncTODO = map_async_opt env v1 in
      let twith = (* "with" *) token env v2 in
      let clause = map_with_clause env v3 in
      let _tcolon = (* ":" *) token env v4 in
      let body = map_suite env v5 in
      With (twith, clause, body)
  | `Func_defi x ->
      let def = map_function_definition env x in
      FunctionDef def
  | `Class_defi x ->
      let def = map_class_definition env x in
      ClassDef def
  | `Deco_defi (v1, v2) ->
      let decorators = Common.map (map_decorator env) v1 in
      let def =
        match v2 with
        | `Class_defi x ->
            let a, b, c, d, _ = map_class_definition env x in
            ClassDef (a, b, c, d, decorators)
        | `Func_defi x ->
            let a, b, c, d, e, _ = map_function_definition env x in
            FunctionDef (a, b, c, d, e, decorators)
      in
      def

and map_elif_clause (env : env) ((v1, v2, v3, v4) : CST.elif_clause) =
  let v1 = (* "elif" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_suite env v4 in
  todo env (v1, v2, v3, v4)

and map_else_clause (env : env) ((v1, v2, v3) : CST.else_clause) =
  let _telse = (* "else" *) token env v1 in
  let _tcolon = (* ":" *) token env v2 in
  let body = map_suite env v3 in
  body

and map_except_clause (env : env) ((v1, v2, v3, v4) : CST.except_clause) :
    excepthandler =
  let texpect = (* "except" *) token env v1 in
  let eopt, nameopt =
    match v2 with
    | Some (v1, v2) ->
        let e = map_type_ env v1 in
        let nameopt =
          match v2 with
          | Some (v1, v2) ->
              let _v1 =
                match v1 with
                | `As tok -> (* "as" *) token env tok
                | `COMMA tok -> (* "," *) token env tok
              in
              (* pfff impose a name here *)
              let v2 = map_type_ env v2 in
              todo env (v1, v2)
          | None -> None
        in
        (Some e, nameopt)
    | None -> (None, None)
  in
  let _tcolon = (* ":" *) token env v3 in
  let body = map_suite env v4 in
  ExceptHandler (texpect, eopt, nameopt, body)

and map_finally_clause (env : env) ((v1, v2, v3) : CST.finally_clause) =
  let tfinally = (* "finally" *) token env v1 in
  let _tcolon = (* ":" *) token env v2 in
  let body = map_suite env v3 in
  (tfinally, body)

and map_function_definition (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.function_definition) :
    function_definition =
  let _asyncTODO = map_async_opt env v1 in
  let tdef = (* "def" *) token env v2 in
  let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v3 in
  let params = map_parameters env v4 in
  let topt =
    match v5 with
    | Some (v1, v2) ->
        let _v1 = (* "->" *) token env v1 in
        let v2 = map_type_ env v2 in
        Some v2
    | None -> None
  in
  let _tcolon = (* ":" *) token env v6 in
  let body = map_suite env v7 in
  (tdef, id, params, topt, body, [])

and map_module_ (env : env) (xs : CST.module_) : stmt list =
  List.concat_map (map_statement env) xs

and map_statement (env : env) (x : CST.statement) : stmt list =
  match x with
  | `Simple_stmts x -> map_simple_statements env x
  | `Choice_if_stmt x -> [ map_compound_statement env x ]

and map_suite (env : env) (x : CST.suite) : stmt list =
  match x with
  | `Simple_stmts x -> map_simple_statements env x
  | `Indent_blk (v1, v2) ->
      let _v1 = (* indent *) token env v1 in
      let v2 = map_block env v2 in
      v2
  | `Nl tok ->
      let _ = (* newline *) token env tok in
      []

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_python.Parse.file file)
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      try map_module_ env cst with
      | Failure "not implemented" as exn ->
          let e = Exception.catch exn in
          H.debug_sexp_cst_after_error (CST.sexp_of_module_ cst);
          Exception.reraise e)
