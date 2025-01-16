(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
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
open Fpath_.Operators
module CST = Tree_sitter_python.CST
module H = Parse_tree_sitter_helpers
open AST_python

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Python parser using tree-sitter-lang/semgrep-python and converting
 * to ../ast/AST_Python.ml
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
let fb = Tok.unsafe_fake_bracket
let invalid () = raise (Tok.NoTokenLocation "Invalid program")

(* AST builders helpers
 * less: could be moved in AST_Python.ml to factorize things with
 * parser_python.mly
 *)

let name_of_id id = Name (id, no_ctx)

let single_or_tuple e xs =
  match xs with
  | [] -> e
  | _ -> Tuple (CompList (fb (e :: xs)), no_ctx)

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-python/Boilerplate.ml *)

(* Disable warnings against unused variables *)
[@@@warning "-26"]

let map_keyword_identifier (env : env) (x : CST.keyword_identifier) : name =
  match x with
  | `Choice_print x -> (
      match x with
      | `Print tok -> (* "print" *) str env tok
      | `Exec tok -> (* "exec" *) str env tok
      | `Async tok -> (* "async" *) str env tok
      | `Await tok -> (* "await" *) str env tok)
  | `Choice_type x -> (
      match x with
      | `Type tok -> (* "type" *) str env tok
      | `Match tok -> (* "match" *) str env tok)

let map_escape_interpolation (env : env) (x : CST.escape_interpolation) =
  str env x

let map_import_prefix (env : env) (xs : CST.import_prefix) : tok list =
  List_.map (token env (* "." *)) xs

let map_anon_choice_STAR_f834b26 (env : env) (x : CST.anon_choice_STAR_f834b26)
    =
  match x with
  | `STAR tok -> (* "*" *) token env tok
  | `STARSTAR tok -> (* "**" *) token env tok

let map_dotted_name (env : env) ((v1, v2) : CST.dotted_name) : dotted_name =
  let v1 = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "." *) token env v1 in
        let v2 =
          (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v2
        in
        v2)
      v2
  in
  v1 :: v2

let map_anon_choice_int_e7b97da (env : env) (x : CST.anon_choice_int_e7b97da) =
  match x with
  | `Int tok ->
      let s, tk = (* integer *) str env tok in
      Int (Parsed_int.parse (s, tk))
  | `Float tok ->
      let s, tk = (* float *) str env tok in
      Float (float_of_string_opt s, tk)

let map_named_expression_lhs (env : env) (x : CST.named_expression_lhs) =
  match x with
  | `Id tok -> (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok
  | `Choice_choice_print x -> map_keyword_identifier env x

let rec map_anon_choice_id_9e93682 (env : env) (x : CST.anon_choice_id_9e93682)
    =
  match x with
  | `Id tok ->
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok in
      Name (id, no_ctx)
  | `Choice_choice_print x ->
      let id = map_keyword_identifier env x in
      Name (id, no_ctx)
  | `Subs x -> map_subscript env x
  | `Attr x -> map_attribute env x

and map_anon_choice_id_9e93682_pattern (env : env)
    (x : CST.anon_choice_id_9e93682) =
  match x with
  | `Id tok ->
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok in
      PatName id
  | `Choice_choice_print x ->
      let id = map_keyword_identifier env x in
      PatName id
  (* This is invalid for the same reason given by the caller, `map_list_splat_pattern`.
     This function is for both parameters and patterns.
     But, you cannot have a parameter or pattern with a subscript.
  *)
  | `Subs _x -> invalid ()
  | `Attr x -> map_attribute_pattern env x

and map_anon_choice_pair_002ffed (env : env) (x : CST.anon_choice_pair_002ffed)
    : dictorset_elt =
  match x with
  | `Pair x ->
      let k, v = map_pair env x in
      KeyVal (k, v)
  | `Dict_splat x ->
      let _tpow, e = map_dictionary_splat env x in
      PowInline e

and map_anon_choice_exp_03d361f (env : env) (x : CST.anon_choice_exp_03d361f) :
    expr =
  match x with
  | `Exp x -> map_expression env x
  | `Yield x -> map_yield env x
  | `List_splat x ->
      let _tstar, e = map_list_splat env x in
      ExprStar e
  | `Paren_list_splat x -> map_parenthesized_list_splat env x

and map_expr_opt env v =
  match v with
  | Some x -> Some (map_expression env x)
  | None -> None

and map_anon_choice_exp_a577897 (env : env) (x : CST.anon_choice_exp_a577897) :
    slice =
  match x with
  | `Exp x ->
      let e = map_expression env x in
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

and map_anon_choice_exp_aad5b2d (env : env) (x : CST.anon_choice_exp_aad5b2d) :
    argument =
  match x with
  | `Exp x ->
      let e = map_expression env x in
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
      let id = map_named_expression_lhs env v1 in
      let _teq = (* "=" *) token env v2 in
      let e = map_expression env v3 in
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
        let arg = map_anon_choice_exp_aad5b2d env v1 in
        let args =
          List_.map
            (fun (v1, v2) ->
              let _tcomma = (* "," *) token env v1 in
              let arg = map_anon_choice_exp_aad5b2d env v2 in
              arg)
            v2
        in
        arg :: args
    | None -> []
  in
  let _ = map_trailing_comma env v3 in
  let rp = (* ")" *) token env v4 in
  (lp, args, rp)

(* This is a `value_pattern` as described by PEP 634: https://peps.python.org/pep-0634/
   You should only be able to have a `NAME` at the very end.
   So reject if it contains anything else.
*)
and map_attribute_pattern (env : env) ((v1, v2, v3) : CST.attribute) : pattern =
  match v1 with
  | `Attr x ->
      let p = map_attribute_pattern env x in
      let tdot = (* "." *) token env v2 in
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v3 in
      PatAttribute (p, tdot, id)
  | `Id id -> PatName (str env id)
  | _ -> invalid ()

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
      let v1 = map_expression env v1 in
      let v2 = (* "and" *) token env v2 in
      let v3 = map_expression env v3 in
      BoolOp ((And, v2), [ v1; v3 ])
  | `Exp_or_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "or" *) token env v2 in
      let v3 = map_expression env v3 in
      BoolOp ((Or, v2), [ v1; v3 ])

and map_collection_elements (env : env) ((v1, v2, v3) : CST.collection_elements)
    : expr list =
  let v1 = map_anon_choice_exp_03d361f env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_exp_03d361f env v2 in
        v2)
      v2
  in
  let _ = map_trailing_comma env v3 in
  v1 :: v2

and map_comprehension_clauses (env : env) ((v1, v2) : CST.comprehension_clauses)
    : for_if list =
  let v1 = map_for_in_clause env v1 in
  let v2 =
    List_.map
      (fun x ->
        match x with
        | `For_in_clause x -> map_for_in_clause env x
        | `If_clause x -> map_if_clause env x)
      v2
  in
  v1 :: v2

and map_concatenated_string (env : env) ((v1, v2) : CST.concatenated_string) =
  let v1 = map_string_ env v1 in
  let v2 = List_.map (map_string_ env) v2 in
  v1 :: v2

and map_dictionary_splat (env : env) ((v1, v2) : CST.dictionary_splat) =
  let v1 = (* "**" *) token env v1 in
  let v2 = map_expression env v2 in
  (v1, v2)

(* Misleadingly, this is called "dictionary_splat_pattern", but only
   occurs in parameter position.
*)
and map_dictionary_splat_pattern (env : env)
    ((v1, v2) : CST.dictionary_splat_pattern) =
  let v1 = (* "**" *) token env v1 in
  let v2 =
    match v2 with
    | `Id tok ->
        let id =
          (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok
        in
        Name (id, no_ctx)
    | `Choice_choice_print x ->
        let id = map_keyword_identifier env x in
        Name (id, no_ctx)
    (* I am not convinced this is possible.
       This would mean a program like
       def foo(x, *y[10]):
         pass
       I find it more likely that the grammar is wrong.
       There are no test cases for this in `tree-sitter-python`.
    *)
    | `Subs _x -> invalid ()
    | `Attr x -> map_attribute env x
  in
  (v1, v2)

(* This is if we are looking for only a single identifier. *)
and map_dictionary_splat_pattern_to_id_single (env : env)
    ((v1, v2) : CST.dictionary_splat_pattern) =
  match map_dictionary_splat_pattern env (v1, v2) with
  | v1, Name (id, _) -> (v1, id)
  | _ -> invalid ()

and map_expression (env : env) (x : CST.expression) : expr =
  match x with
  | `Comp_op (v1, v2) ->
      let e = map_primary_expression env v1 in
      let xs =
        List_.map
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
      Compare (e, xs |> List_.map fst, xs |> List_.map snd)
  | `Not_op (v1, v2) ->
      let v1 = (* "not" *) token env v1 in
      let v2 = map_expression env v2 in
      UnaryOp ((Not, v1), v2)
  | `Bool_op x -> map_boolean_operator env x
  | `Lambda (v1, v2, v3, v4) ->
      let tlambda = (* "lambda" *) token env v1 in
      let params =
        match v2 with
        | Some x -> map_lambda_parameters env x
        | None -> []
      in
      let tcolon = (* ":" *) token env v3 in
      let body = map_expression env v4 in
      Lambda (tlambda, params, tcolon, body)
  | `Prim_exp x -> map_primary_expression env x
  | `Cond_exp (v1, v2, v3, v4, v5) ->
      let v1 = map_expression env v1 in
      let _v2 = (* "if" *) token env v2 in
      let v3 = map_expression env v3 in
      let _v4 = (* "else" *) token env v4 in
      let v5 = map_expression env v5 in
      IfExp (v3, v1, v5)
  | `Named_exp (v1, v2, v3) ->
      (* TODO? pfff allows any expr on lhs *)
      let v1 = map_named_expression_lhs env v1 in
      let teq = (* ":=" *) token env v2 in
      let e = map_expression env v3 in
      NamedExpr (name_of_id v1, teq, e)
  | `As_pat_ (v1, v2, v3) ->
      (* This site should be guarded, so it shouldn't be reached ideally.
         As-patterns are not truly expressions, but they can occur in the context of a `with` or
         `except`.
      *)
      let v1 = map_expression env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 = map_expression env v3 in
      invalid ()

and map_expression_list (env : env) ((v1, v2) : CST.expression_list) : expr list
    =
  let v1 = map_expression env v1 in
  let v2 =
    match v2 with
    | `COMMA tok ->
        let _trailing = (* "," *) token env tok in
        []
    | `Rep1_COMMA_exp_opt_COMMA (v1, v2) ->
        let v1 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_expression env v2 in
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
  | `Exp x -> map_expression env x
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
  | `Exp x -> map_expression env x
  | `Exp_list x ->
      let xs = map_expression_list env x in
      Tuple (CompList (fb xs), no_ctx)

and map_expressions2 (env : env) (x : CST.expressions) : expr list =
  match x with
  | `Exp x -> [ map_expression env x ]
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
    List_.map
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

and map_f_expression (env : env) (x : CST.f_expression) : expr list =
  match x with
  | `Exp x -> [ map_expression env x ]
  | `Exp_list x -> map_expression_list env x
  | `Pat_list x -> map_pattern_list_expr env x
  | `Yield x -> [ map_yield env x ]

and map_format_specifier (env : env) ((v1, v2) : CST.format_specifier) :
    expr list =
  let _tcolon = (* ":" *) token env v1 in
  let xs =
    List.concat_map
      (fun x ->
        match x with
        | `Tok_prec_p1_pat_a2d1fce tok ->
            let x = (* [^{}\n]+ *) str env tok in
            [ Literal (Str x) ]
        | `Interp x ->
            let _l, x, _r = map_interpolation env x in
            x)
      v2
  in
  xs

and map_generator_expression (env : env)
    ((v1, v2, v3, v4) : CST.generator_expression) : expr =
  let lp = (* "(" *) token env v1 in
  let e = map_expression env v2 in
  let forifs = map_comprehension_clauses env v3 in
  let rp = (* ")" *) token env v4 in
  Tuple (CompForIf (lp, (e, forifs), rp), no_ctx)

and map_if_clause (env : env) ((v1, v2) : CST.if_clause) : for_if =
  let _tif = (* "if" *) token env v1 in
  let e = map_expression env v2 in
  CompIf e

and map_interpolation (env : env) ((v1, v2, v3, v4, v5, v6) : CST.interpolation)
    =
  let lb = (* "{" *) token env v1 in
  let e = map_f_expression env v2 in
  let () =
    match v3 with
    | Some _tok -> (* "=" *) ()
    | None -> ()
  in
  let _expressionconv_opt =
    match v4 with
    | Some tok -> Some ((* pattern ![a-z] *) str env tok)
    | None -> None
  in
  let _format_opt =
    match v5 with
    | Some x -> Some (map_format_specifier env x)
    | None -> None
  in
  let rb = (* "}" *) token env v6 in
  (lb, e, rb)

and map_lambda_parameters (env : env) (x : CST.lambda_parameters) =
  map_parameters_ env x

(* A left-hand side is technically comprised of `pattern` nodes, but
   `map_left_hand_side` returns an `expr`. What gives?
   The `tree-sitter-python` grammar allows `pattern`s to appear in numerous
   places, like in LHS position (assignments, list comprehensions, casts),
   as well as the more canonical `match` and `case` position.
   While these share a node in the grammar, the rules around what is allowed
   in either position are different.

   For instance, we may have a valid Python program as `x[10] = 2`, where the
   left-hand side reduces to a `pattern`, but we may _not_ have
   ```
   match x:
     case y[10]:
       pass
   ```

   So, in order to make this distinction apparent, we spawn a `pattern` type,
   which is only ever employed in the `case` position, and restricts the things
   which are allowed to appear there.

   Separately, where we see a `pattern` node (often from a `left_hand_side`),
   we parse directly to an expression, which allows for things like subscripts.
*)
and map_left_hand_side (env : env) (x : CST.left_hand_side) : expr =
  match x with
  | `Pat x -> map_pattern_expr env x
  | `Pat_list (v1, v2) ->
      let expr = map_pattern_expr env v1 in
      let exprs =
        match v2 with
        | `COMMA tok ->
            let _trailing = (* "," *) token env tok in
            []
        | `Rep1_COMMA_pat_opt_COMMA (v1, v2) ->
            let v1 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_pattern_expr env v2 in
                  v2)
                v1
            in
            let _v2 = map_trailing_comma env v2 in
            v1
      in
      single_or_tuple expr exprs

and map_list_splat (env : env) ((v1, v2) : CST.list_splat) =
  let v1 = (* "*" *) token env v1 in
  let v2 = map_expression env v2 in
  (v1, v2)

and map_list_splat_pattern_expr (env : env) ((v1, v2) : CST.list_splat_pattern)
    =
  let v1 = (* "*" *) token env v1 in
  let v2 = map_anon_choice_id_9e93682 env v2 in
  (v1, v2)

(* Unlike `dictionary_splat_pattern`, this actually could relate to patterns.
   This function is for both parameters and patterns.

   This is unfortunate, because this choice contains `attribute`, which leads
   to `primary_expression`, meaning there is an embedding of any arbitrary
   expression inside of parameters and patterns.

   This is obviously unrealistic, as there are many expressions which should
   not be permissible in either place.
*)
and map_list_splat_pattern (env : env) ((v1, v2) : CST.list_splat_pattern) =
  let v1 = (* "*" *) token env v1 in
  let v2 = map_anon_choice_id_9e93682_pattern env v2 in
  (v1, v2)

(* This is if we permit dotted names *)
and map_list_splat_pattern_to_id (env : env) ((v1, v2) : CST.list_splat_pattern)
    =
  match map_list_splat_pattern env (v1, v2) with
  | v1, PatName id -> (v1, id)
  | _ -> invalid ()

(* This is if we expect only a single identifier *)
and map_list_splat_pattern_to_id_single (env : env)
    ((v1, v2) : CST.list_splat_pattern) =
  match map_list_splat_pattern env (v1, v2) with
  | v1, PatName id -> (v1, id)
  | _ -> invalid ()

and map_pair (env : env) ((v1, v2, v3) : CST.pair) =
  let key = map_expression env v1 in
  let _v2 = (* ":" *) token env v2 in
  let val_ = map_expression env v3 in
  (key, val_)

and map_parameter (env : env) (x : CST.parameter) : parameter =
  match x with
  | `Id tok ->
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok in
      ParamPattern (PatternName id, None)
  | `Typed_param (v1, _v2TODO, v3) -> (
      let ty = map_type_ env v3 in
      match v1 with
      | `Id tok ->
          let id =
            (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok
          in
          ParamPattern (PatternName id, Some ty)
      | `List_splat_pat x ->
          let tstar, id = map_list_splat_pattern_to_id_single env x in
          ParamStar (tstar, (id, Some ty))
      | `Dict_splat_pat x ->
          let tpow, id = map_dictionary_splat_pattern_to_id_single env x in
          ParamPow (tpow, (id, Some ty)))
  | `Defa_param (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Id tok ->
            (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *)
            PatternName (str env tok)
        | `Tuple_pat_ x -> map_tuple_pattern_to_parameter env x
      in
      let _teq = (* "=" *) token env v2 in
      let e = map_expression env v3 in
      ParamDefault ((v1, None), e)
  | `Typed_defa_param (v1, v2, v3, v4, v5) ->
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v1 in
      let _tcolon = (* ":" *) token env v2 in
      let ty = map_type_ env v3 in
      let _teq = (* "=" *) token env v4 in
      let e = map_expression env v5 in
      ParamDefault ((PatternName id, Some ty), e)
  | `List_splat_pat x ->
      let tstar, id = map_list_splat_pattern_to_id_single env x in
      ParamStar (tstar, (id, None))
  | `Tuple_pat_ x ->
      (* bTODO: This appears to be busted, and I think it's tree-sitter's fault.
         This allows a tuple pattern to appear as a parameter. Notably, tuple patterns
         may contain list patterns. This is OK for matching, but functions in python
         are treated differently, and cannot decompose on a list pattern.
         So this is overly permissive, and makes it a pain to do this translation. *)
      let param_pat = map_tuple_pattern_to_parameter env x in
      ParamPattern (param_pat, None)
  | `Kw_sepa tok ->
      let t = (* "*" *) token env tok in
      ParamSingleStar t
  | `Posi_sepa tok ->
      (* "/" *)
      let t = token env tok in
      ParamSlash t
  | `Dict_splat_pat x ->
      let tstar, id = map_dictionary_splat_pattern_to_id_single env x in
      ParamPow (tstar, (id, None))

and map_anon_choice_key_value_pat_9cde426 (env : env)
    (x : CST.anon_choice_key_value_pat_9cde426) =
  match x with
  | `Key_value_pat (v1, v2, v3) ->
      let v1 = map_simple_pattern env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_case_pattern env v3 in
      PatKeyVal (v1, v2, v3)
  | `Splat_pat x -> map_splat_pattern env x

and map_case_pattern (env : env) (x : CST.case_pattern) : pattern =
  match x with
  | `As_pat (v1, v2, v3) ->
      let v1 = map_case_pattern env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 =
        (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) str env v3
      in
      PatAs (v1, v2, v3)
  | `Kw_pat (v1, v2, v3) ->
      let v1 =
        (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) str env v1
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_simple_pattern env v3 in
      PatKeyVal (PatName v1, v2, v3)
  | `Simple_pat x -> map_simple_pattern env x

and map_anon_case_pat_rep_COMMA_case_pat_opt_COMMA_0f1ba58 (env : env)
    ((v1, v2, v3) : CST.anon_case_pat_rep_COMMA_case_pat_opt_COMMA_0f1ba58) =
  let v1 = map_case_pattern env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_case_pattern env v2 in
        v2)
      v2
  in
  let _v3 =
    match v3 with
    | Some tok -> Some ((* "," *) token env tok)
    | None -> None
  in
  v1 :: v2

and map_simple_pattern (env : env) (x : CST.simple_pattern) : pattern =
  match x with
  | `Class_pat (v1, v2, v3, v4) ->
      let v1 = map_dotted_name env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> map_anon_case_pat_rep_COMMA_case_pat_opt_COMMA_0f1ba58 env x
        | None -> []
      in
      let v4 = (* ")" *) token env v4 in
      PatConstructor (v1, (v2, v3, v4))
  (* I don't think this should be able to occur here.
     It should only be *)
  | `Splat_pat x ->
      (* ignore (map_splat_pattern env x); *)
      map_splat_pattern env x
  | `Union_pat (v1, v2) ->
      let v1 = map_simple_pattern env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let v1 = (* "|" *) token env v1 in
            let v2 = map_simple_pattern env v2 in
            v2)
          v2
      in
      PatDisj (v1 :: v2)
  | `List_pat (v1, v2, v3) ->
      let l = (* "[" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_anon_case_pat_rep_COMMA_case_pat_opt_COMMA_0f1ba58 env x
        | None -> []
      in
      let r = (* "]" *) token env v3 in
      (* List (CompList (l, v2, r), no_ctx) *)
      PatList (l, v2, r)
  | `Tuple_pat (v1, v2, v3) ->
      let l = (* "(" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_anon_case_pat_rep_COMMA_case_pat_opt_COMMA_0f1ba58 env x
        | None -> []
      in
      let r = (* ")" *) token env v3 in
      PatTuple (l, v2, r)
  | `Dict_pat (v1, v2, v3) ->
      let l = (* "{" *) token env v1 in
      let xs =
        match v2 with
        | Some (v1, v2, v3) ->
            let v1 = map_anon_choice_key_value_pat_9cde426 env v1 in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = map_anon_choice_key_value_pat_9cde426 env v2 in
                  v2)
                v2
            in
            let v3 =
              match v3 with
              | Some tok -> Some ((* "," *) token env tok)
              | None -> None
            in
            v1 :: v2
        | None -> []
      in
      let r = (* "}" *) token env v3 in
      PatDict (l, xs, r)
  | `True tok ->
      let t = (* "True" *) token env tok in
      PatLiteral (Bool (true, t))
  | `False tok ->
      let t = (* "False" *) token env tok in
      PatLiteral (Bool (false, t))
  | `Str x ->
      let t1, s, t2 = map_string_ env x in
      PatInterpolatedString (t1, s, t2)
  | `Conc_str x ->
      let xs = map_concatenated_string env x in
      PatConcatenatedString (List_.map (fun (_, x, _) -> x) xs |> List_.flatten)
      (* map_concatenated_string env x *)
  | `None tok -> PatLiteral (None_ ((* "None" *) token env tok))
  | `Opt_DASH_choice_int (v1, v2) ->
      let is_neg =
        match v1 with
        | Some _tok -> (* "-" *) true
        | None -> false
      in
      let v2 = map_anon_choice_int_e7b97da env v2 in
      if is_neg then
        match v2 with
        | Int pi -> PatLiteral (Num (Int (Parsed_int.neg pi)))
        | LongInt pi -> PatLiteral (Num (LongInt (Parsed_int.neg pi)))
        | Float (f, t) -> (
            match f with
            | None -> PatLiteral (Num (Float (f, t)))
            | Some f -> PatLiteral (Num (Float (Some (-.f), t))))
        | Imag _ -> PatLiteral (Num v2)
      else PatLiteral (Num v2)
  (* why on earth is this allowed *)
  (* python apparently allows pattern matching on complex numbersm like `1+1j`
     https://peps.python.org/pep-0622/#literal-patterns
  *)
  | `Comp_pat (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | Some tok -> Some ((* "-" *) token env tok)
        | None -> None
      in
      let v2 = map_anon_choice_int_e7b97da env v2 in
      let v3 =
        match v3 with
        | `PLUS tok -> (* "+" *) token env tok
        | `DASH tok -> (* "-" *) token env tok
      in
      let v4 = map_anon_choice_int_e7b97da env v4 in
      PatComplex (v1, v2, v3, v4)
  | `Dotted_name (v1, v2) ->
      let v1 = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let v1 = (* "." *) token env v1 in
            let v2 =
              (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v2
            in
            (v1, v2))
          v2
      in
      List.fold_left
        (fun acc (tdot, x) -> PatAttribute (acc, tdot, x))
        (PatName v1) v2
  | `X__ tok -> PatUnderscore ((* "_" *) token env tok)

and map_parameters_ (env : env) ((v1, v2, v3) : CST.parameters_) =
  let v1 = map_parameter env v1 in
  let v2 =
    List_.map
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

and map_pattern_to_parameter (env : env) (x : CST.pattern) : param_pattern =
  match x with
  | `Id tok ->
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok in
      PatternName id
  | `Choice_choice_print x ->
      let id = map_keyword_identifier env x in
      PatternName id
  (* These are not parameters. *)
  | `Subs _
  | `List_pat_ _
  | `Attr _ ->
      raise (Tok.NoTokenLocation "")
  | `List_splat_pat x ->
      (* Via the Python 3 grammar, you can only have a pow in a pattern if the next
         is just a NAME.
      *)
      let tstar, id = map_list_splat_pattern_to_id env x in
      invalid ()
  (* Tuples are not parameters, after the first. *)
  | `Tuple_pat_ _x -> invalid ()

(* Patterns which occur in `expr` position. *)
and map_pattern_expr (env : env) (x : CST.pattern) : expr =
  match x with
  | `Id tok ->
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok in
      name_of_id id
  | `Choice_choice_print x ->
      let id = map_keyword_identifier env x in
      name_of_id id
  | `Subs x -> map_subscript env x
  | `Attr x -> map_attribute env x
  | `List_splat_pat x ->
      (* Via the Python 3 grammar, you can only have a pow in a pattern if the next
         is just a NAME.
      *)
      let _tstar, splatted_expr = map_list_splat_pattern_expr env x in
      ExprStar splatted_expr
  | `Tuple_pat_ x ->
      let lp, xs, rp = map_tuple_pattern_expr env x in
      Tuple (CompList (lp, xs, rp), no_ctx)
  | `List_pat_ (v1, v2, v3) ->
      let lb = (* "[" *) token env v1 in
      let xs =
        match v2 with
        | Some x -> map_patterns_expr env x
        | None -> []
      in
      let rb = (* "]" *) token env v3 in
      List (CompList (lb, xs, rb), no_ctx)

and map_pattern_list_expr (env : env) ((v1, v2) : CST.pattern_list) =
  let v1 = map_pattern_expr env v1 in
  let v2 =
    match v2 with
    | `COMMA _tok ->
        (* "," *)
        []
    | `Rep1_COMMA_pat_opt_COMMA (v1, v2) ->
        let v1 =
          List_.map
            (fun (v1, v2) ->
              let v1 = (* "," *) token env v1 in
              let v2 = map_pattern_expr env v2 in
              v2)
            v1
        in
        let _v2 =
          match v2 with
          | Some tok -> Some ((* "," *) token env tok)
          | None -> None
        in
        v1
  in
  v1 :: v2

and map_patterns_expr (env : env) ((v1, v2, v3) : CST.patterns) : expr list =
  let v1 = map_pattern_expr env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_pattern_expr env v2 in
        v2)
      v2
  in
  (* less? trailing comma important when on lhs? *)
  let _ = map_trailing_comma env v3 in
  v1 :: v2

and map_patterns_to_parameters (env : env) ((v1, v2, v3) : CST.patterns) :
    param_pattern list =
  let v1 = map_pattern_to_parameter env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_pattern_to_parameter env v2 in
        v2)
      v2
  in
  (* less? trailing comma important when on lhs? *)
  let _ = map_trailing_comma env v3 in
  v1 :: v2

and map_primary_expression (env : env) (x : CST.primary_expression) : expr =
  match x with
  | `Await (v1, v2) ->
      let v1 = (* "await" *) token env v1 in
      let v2 = map_primary_expression env v2 in
      Await (v1, v2)
  | `Bin_op x -> map_binary_operator env x
  | `Id tok ->
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok in
      name_of_id id
  | `Choice_choice_print x ->
      let id = map_keyword_identifier env x in
      name_of_id id
  | `Str x ->
      let t1, s, t2 = map_string_ env x in
      InterpolatedString (t1, s, t2)
  | `Conc_str (v1, v2) ->
      let _, v1, _ = map_string_ env v1 in
      let v2 = List_.map (map_string_ env) v2 in
      ConcatenatedString
        (v1 @ (List_.map (fun (_, x, _) -> x) v2 |> List_.flatten))
  | `Int tok ->
      let s, tk = (* integer *) str env tok in
      Literal (Num (Int (Parsed_int.parse (s, tk))))
  | `Float tok ->
      let s, tk = (* float *) str env tok in
      Literal (Num (Float (float_of_string_opt s, tk)))
  | `True tok ->
      let t = (* "True" *) token env tok in
      Literal (Bool (true, t))
  | `False tok ->
      let t = (* "False" *) token env tok in
      Literal (Bool (false, t))
  | `None tok ->
      let t = (* "None" *) token env tok in
      Literal (None_ t)
  | `Un_op (v1, v2) ->
      let op =
        match v1 with
        | `PLUS tok -> (UAdd, (* "+" *) token env tok)
        | `DASH tok -> (USub, (* "-" *) token env tok)
        | `TILDE tok -> (Invert, (* "~" *) token env tok)
      in
      let e = map_primary_expression env v2 in
      UnaryOp (op, e)
  | `Attr (v1, v2, v3) -> map_attribute env (v1, v2, v3)
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
      let e = map_expression env v2 in
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
              List_.map
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
      let ys = xs |> List_.map (fun e -> Key e) in
      DictOrSet (CompList (l, ys, r))
  | `Set_comp (v1, v2, v3, v4) ->
      let l = (* "{" *) token env v1 in
      let e = map_expression env v2 in
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
      let lp = (* "(" *) token env v1 in
      let e =
        match v2 with
        | `Exp x -> map_expression env x
        | `Yield x -> map_yield env x
      in
      let rp = (* ")" *) token env v3 in
      ParenExpr (lp, e, rp)
  | `Gene_exp x ->
      let x = map_generator_expression env x in
      x
  | `Ellips tok ->
      let t = (* "..." *) token env tok in
      Ellipsis t
  | `List_splat_pat x ->
      (* The grammar is imprecise here and permits splats as arbitrary
         expressions, when in reality they are only permitted to appear in
         certain areas.
      *)
      let _tstar, exp = map_list_splat_pattern_expr env x in
      ExprStar exp

and map_string_ (env : env) ((v1, v2, v3) : CST.string_) :
    interpolated list bracket =
  let str_start = (* string_start *) token env v1 in
  let xs =
    List.concat_map
      (fun x ->
        match x with
        | `Interp x ->
            let _lb, e, _rb = map_interpolation env x in
            e
        | `Str_content x ->
            let s = map_string_content env x in
            s)
      v2
  in
  let str_end = (* string_end *) token env v3 in
  (str_start, xs, str_end)

and map_subscript (env : env) ((v1, v2, v3, v4, v5, v6) : CST.subscript) =
  let e = map_primary_expression env v1 in
  let l = (* "[" *) token env v2 in
  let slice = map_anon_choice_exp_a577897 env v3 in
  let slices =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_exp_a577897 env v2 in
        v2)
      v4
  in
  let _ = map_trailing_comma env v5 in
  let r = (* "]" *) token env v6 in
  Subscript (e, (l, slice :: slices, r), no_ctx)

and map_tuple_pattern_expr (env : env) ((v1, v2, v3) : CST.tuple_pattern_) :
    expr list bracket =
  let lp = (* "(" *) token env v1 in
  let xs =
    match v2 with
    | Some x -> map_patterns_expr env x
    | None -> []
  in
  let rp = (* ")" *) token env v3 in
  (lp, xs, rp)

and map_tuple_pattern_to_parameter (env : env)
    ((_v1TODO, v2, _v3TODO) : CST.tuple_pattern_) : param_pattern =
  let xs =
    match v2 with
    | Some x -> map_patterns_to_parameters env x
    | None -> []
  in
  PatternTuple xs

and map_type_ (env : env) (x : CST.type_) : type_ =
  match x with
  | `Exp x -> map_expression env x
  | `Splat_type (v1, v2) ->
      let v1 = map_anon_choice_STAR_f834b26 env v1 in
      let v2 =
        (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) str env v2
      in
      ExprStar (Name (v2, no_ctx))
  | `Gene_type (v1, v2) -> (
      let l, v2, r = map_type_parameter env v2 in
      match (v1, v2) with
      | `Id tok, _ ->
          let id =
            (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *)
            str env tok
          in
          let slices = List_.map (fun x -> Index x) v2 in
          Subscript (Name (id, no_ctx), (l, slices, r), no_ctx)
      | `Type _tok, t :: _ ->
          (* "type" *)
          t
      (* what would a type of `type[]` mean? *)
      | `Type _tok, [] -> invalid ())
  | `Union_type (v1, v2, v3) ->
      let v1 = map_type_ env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_type_ env v3 in
      BinOp (v1, (BitOr, v2), v3)
  | `Cons_type (v1, v2, v3) ->
      let v1 = map_type_ env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_type_ env v3 in
      ConstrainedType (v1, v2, v3)
  | `Member_type (v1, v2, v3) ->
      let v1 = map_type_ env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 =
        (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) str env v3
      in
      Attribute (v1, v2, v3, no_ctx)

and map_type_parameter (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameter) :
    expr list bracket =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_type_ env v2 in
        v2)
      v3
  in
  let _v4 =
    match v4 with
    | Some tok -> Some ((* "," *) token env tok)
    | None -> None
  in
  let v5 = (* "]" *) token env v5 in
  (v1, v2 :: v3, v5)

and map_yield (env : env) ((v1, v2) : CST.yield) : expr =
  let v1 = (* "yield" *) token env v1 in
  let exp_opt, flag =
    match v2 with
    | `From_exp (v1, v2) ->
        let v1 = (* "from" *) token env v1 in
        let v2 = map_expression env v2 in
        (Some v2, true)
    | `Opt_choice_exp opt -> (
        match opt with
        | Some x -> (Some (map_expressions env x), false)
        | None -> (None, false))
  in
  Yield (v1, exp_opt, flag)

and map_splat_pattern (env : env) ((v1, v2) : CST.splat_pattern) : pattern =
  let v1 = map_anon_choice_STAR_f834b26 env v1 in
  let v2 =
    match v2 with
    | `Id tok ->
        PatName
          ((* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok)
    | `X__ tok ->
        (* "_" *)
        PatUnderscore (token env tok)
  in
  PatSplat (v1, v2)

and map_string_content (env : env) (xs : CST.string_content) =
  List_.map
    (function
      | `Esc_interp x ->
          let s = map_escape_interpolation env x in
          Literal (Str s)
      | `Esc_seq tok ->
          let s = (* escape_sequence *) str env tok in
          Literal (Str s)
      | `Not_esc_seq tok ->
          let s = (* "\\" *) str env tok in
          Literal (Str s)
      | `Str_content_ tok ->
          let s = (* string_content *) str env tok in
          Literal (Str s))
    xs

let map_relative_import (env : env) ((v1, v2) : CST.relative_import) :
    module_name =
  let v1 = map_import_prefix env v1 in
  let v2 = Option.map (map_dotted_name env) v2 in
  match (v1, v2) with
  (* This case is an empty import and cannot happen. *)
  | [], None -> invalid ()
  (* This case is taken directly from the pfff parser. I do not know why it does that. *)
  | fst :: _restTODO, None -> ([ ("", fst (*TODO*)) ], Some v1)
  | [], Some dname -> (dname, None)
  | _, Some dname -> (dname, Some v1)

let map_with_item (env : env) (v1 : CST.with_item) =
  match v1 with
  | `As_pat_ (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 = map_expression env v3 in
      (v1, Some v3)
  | _ -> (map_expression env v1, None)

let rec map_assignment (env : env) ((v1, v2) : CST.assignment) =
  let lhs = map_left_hand_side env v1 in
  match v2 with
  | `EQ_right_hand_side (v1, v2) ->
      let v1 = (* "=" *) token env v1 in
      let vars, tokopt, v2 = map_right_hand_side env v2 in
      let tok =
        match tokopt with
        | None -> v1
        | Some res -> res
      in
      ((lhs, None) :: vars, tok, v2)
  (* we can only reach here if we called `map_assignment` through at least one assignment already
     something like x = y : Int is not valid syntax, so we should reject here
  *)
  | `COLON_type (_v1, _v2) -> invalid ()
  | `COLON_type_EQ_right_hand_side (v1, v2, v3, v4) ->
      let v1 = (* ":" *) token env v1 in
      let v2 = map_type_ env v2 in
      let v3 = (* "=" *) token env v3 in
      let vars, tokopt, v4 = map_right_hand_side env v4 in
      let tok =
        match tokopt with
        | None -> v3
        | Some res -> res
      in
      ((lhs, Some (v1, v2)) :: vars, tok, v4)

and map_assignment_final (env : env) ((v1, v2) : CST.assignment) =
  let lhs = map_left_hand_side env v1 in
  match v2 with
  | `COLON_type (v1, v2) -> Cast (lhs, token env v1, map_type_ env v2)
  | _ ->
      let vars, tokopt, expr = map_assignment env (v1, v2) in
      Assign (vars, tokopt, expr)

and map_augmented_assignment (env : env)
    ((v1, v2, v3) : CST.augmented_assignment) =
  let lhs = map_left_hand_side env v1 in
  let wrap =
    match v2 with
    | `PLUSEQ tok -> (* "+=" *) (Add, token env tok)
    | `DASHEQ tok -> (* "-=" *) (Sub, token env tok)
    | `STAREQ tok -> (* "*=" *) (Mult, token env tok)
    | `SLASHEQ tok -> (* "/=" *) (Div, token env tok)
    | `ATEQ tok -> (* "@=" *) (MatMult, token env tok)
    | `SLASHSLASHEQ tok -> (* "//=" *) (FloorDiv, token env tok)
    | `PERCEQ tok -> (* "%=" *) (Mod, token env tok)
    | `STARSTAREQ tok -> (* "**=" *) (Pow, token env tok)
    | `GTGTEQ tok -> (* ">>=" *) (RShift, token env tok)
    | `LTLTEQ tok -> (* "<<=" *) (LShift, token env tok)
    | `AMPEQ tok -> (* "&=" *) (BitAnd, token env tok)
    | `HATEQ tok -> (* "^=" *) (BitXor, token env tok)
    | `BAREQ tok -> (* "|=" *) (BitOr, token env tok)
  in
  let vars, _, expr = map_right_hand_side env v3 in
  match vars with
  (* The RHS of an augmented assignment cannot be an assignment itself.
     So vars should be empty.
  *)
  | _ :: _ -> invalid ()
  | _ -> AugAssign (lhs, wrap, expr)

and map_right_hand_side (env : env) (x : CST.right_hand_side) =
  match x with
  | `Exp x -> ([], None, map_expression env x)
  | `Exp_list x ->
      let xs = map_expression_list env x in
      ([], None, Tuple (CompList (fb xs), no_ctx))
  | `Assign x ->
      let vars, tok, expr = map_assignment env x in
      (vars, Some tok, expr)
  (* An augmented assignment cannot actually occur as the RHS to an assignment. *)
  | `Augm_assign _x -> invalid ()
  | `Pat_list x ->
      ([], None, Tuple (CompList (fb (map_pattern_list_expr env x)), no_ctx))
  | `Yield x -> ([], None, map_yield env x)

let map_decorator (env : env) ((v1, v2, v3) : CST.decorator) =
  let tat = (* "@" *) token env v1 in
  (* We are looking for a dotted name, so we don't permit other variants. *)
  let expr = map_expression env v2 in
  let _newline = (* newline *) token env v3 in
  (tat, expr)

(* python2? *)
let map_chevron (env : env) ((v1, v2) : CST.chevron) =
  let v1 = (* ">>" *) token env v1 in
  let v2 = map_expression env v2 in
  v2

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

let map_with_clause (env : env) (x : CST.with_clause) (twith : tok)
    (body : stmt list) =
  let combine_mapped_with_items items =
    List_.fold_right
      (fun wclause acc ->
        match acc with
        | None -> Some (With (twith, wclause, body))
        | Some acc -> Some (With (twith, wclause, [ acc ])))
      items None
  in
  match x with
  (* this implies something like
      ```
      with ():
        ...
      ```
  *)
  | `With_item_rep_COMMA_with_item_opt_COMMA
      (`Prim_exp (`Tuple (_, None, _)), [], _) ->
      None
  (* in Python 3.10 you can have multiple with items in a tuple, like this:
     with (
       f() as a,
       g() as b,
     ):
       ...
     which will reach this case instead of the LPAR_with_item case below
     we have preconditions against mapping `As_pat`s directly, so we need to
     destructure a bit here first
  *)
  | `With_item_rep_COMMA_with_item_opt_COMMA
      (`Prim_exp (`Tuple (_, Some (x, xs, _), _)), [], _) ->
      let items =
        List_.map
          (fun x ->
            match x with
            | `Exp x -> map_with_item env x
            | _ -> (map_anon_choice_exp_03d361f env x, None))
          (x :: List_.map snd xs)
      in
      combine_mapped_with_items items
  | `With_item_rep_COMMA_with_item_opt_COMMA (w, ws, _)
  | `LPAR_with_item_rep_COMMA_with_item_opt_COMMA_RPAR (_, w, ws, _, _) ->
      let items =
        List_.map (fun x -> map_with_item env x) (w :: List_.map snd ws)
      in
      combine_mapped_with_items items

let map_expression_statement (env : env) (x : CST.expression_statement) : stmt =
  match x with
  | `Exp x -> ExprStmt (map_expression env x)
  | `Exp_rep_COMMA_exp_opt_COMMA (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_expression env v2 in
            v2)
          v2
      in
      let _ = map_trailing_comma env v3 in
      ExprStmt (single_or_tuple v1 v2)
  | `Assign x -> map_assignment_final env x
  | `Augm_assign x -> map_augmented_assignment env x
  | `Yield x -> ExprStmt (map_yield env x)

let map_print_statement (env : env) (x : CST.print_statement) =
  match x with
  | `Print_chev_rep_COMMA_exp_opt_COMMA (v1, v2, v3, v4) ->
      let v1 = (* "print" *) token env v1 in
      let v2 = map_chevron env v2 in
      let v3 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_expression env v2 in
            v2)
          v3
      in
      let v4 = map_trailing_comma env v4 |> Option.is_some in
      Print (v1, Some v2, v3, v4)
  | `Print_exp_rep_COMMA_exp_opt_COMMA (v1, v2, v3, v4) ->
      let v1 = (* "print" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_expression env v2 in
            v2)
          v3
      in
      let v4 = map_trailing_comma env v4 |> Option.is_some in
      Print (v1, None, v2 :: v3, v4)

let map_import_list (env : env) ((v1, v2, v3) : CST.import_list) =
  let v1 = map_anon_choice_dotted_name_c5c573a env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_dotted_name_c5c573a env v2 in
        v2)
      v2
  in
  let v3 = map_trailing_comma env v3 in
  v1 :: v2

let map_simple_statement (env : env) (x : CST.simple_statement) : stmt list =
  match x with
  | `Future_import_stmt (v1, v2, v3, v4) ->
      let v1 = (* "from" *) token env v1 in
      let v3 = (* "import" *) token env v3 in
      let v4 =
        match v4 with
        | `Import_list x
        | `LPAR_import_list_RPAR (_, x, _) ->
            let xs = map_import_list env x in
            let xs =
              List_.map
                (function
                  | [ name ], y -> (name, y)
                  (* import _ from _ only permits single identifiers in the second list *)
                  | _ -> invalid ())
                xs
            in
            [ ImportFrom (v1, ([ str env v2 ], None), xs) ]
      in
      v4
  | `Import_stmt (v1, v2) ->
      let v1 = (* "import" *) token env v1 in
      let v2 = map_import_list env v2 in
      List_.map (fun (dname, asopt) -> ImportAs (v1, (dname, None), asopt)) v2
  | `Import_from_stmt (v1, v2, v3, v4) -> (
      let tfrom = (* "from" *) token env v1 in
      let path =
        match v2 with
        | `Rela_import x -> map_relative_import env x
        | `Dotted_name x -> (map_dotted_name env x, None)
      in
      let _timport = (* "import" *) token env v3 in
      match v4 with
      | `Wild_import tok ->
          let tok = (* "*" *) token env tok in
          [ ImportAll (tfrom, path, tok) ]
      | `Import_list x
      | `LPAR_import_list_RPAR (_, x, _) ->
          let xs = map_import_list env x in
          let xs =
            List_.map
              (function
                | [ name ], y -> (name, y)
                (* import _ from _ only permits single identifiers in the second list *)
                | _ -> invalid ())
              xs
          in
          [ ImportFrom (tfrom, path, xs) ])
  | `Print_stmt x -> [ map_print_statement env x ]
  | `Assert_stmt (v1, v2, v3) ->
      let tassert = (* "assert" *) token env v1 in
      let test = map_expression env v2 in
      let test2 =
        match v3 with
        | [] -> None
        | [ e ] -> Some (map_expression env (e |> snd))
        (* python only permits two of these at max *)
        | _ -> invalid ()
      in
      [ Assert (tassert, test, test2) ]
  | `Exp_stmt x -> [ map_expression_statement env x ]
  | `Ret_stmt (v1, v2) ->
      let tret = (* "return" *) token env v1 in
      let eopt =
        match v2 with
        | Some x -> Some (map_expressions env x)
        | None -> None
      in
      [ Return (tret, eopt) ]
  | `Delete_stmt (v1, v2) ->
      let tdel = (* "del" *) token env v1 in
      let xs = map_expressions2 env v2 in
      [ Delete (tdel, xs) ]
  | `Raise_stmt (v1, v2, v3) ->
      let traise = (* "raise" *) token env v1 in
      let v2 = Option.map (map_expressions2 env) v2 in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let _tfrom = (* "from" *) token env v1 in
            let v2 = map_expression env v2 in
            Some v2
        | None -> None
      in
      [
        (match v2 with
        | None -> Raise (traise, None)
        | Some [ e ] -> Raise (traise, Some (e, v3))
        (* We're not using v3 in these cases, but the python2 grammar doesn't permit that anyways. *)
        | Some [ e1; e2 ] -> RaisePython2 (traise, e1, Some e2, None)
        | Some [ e1; e2; e3 ] -> RaisePython2 (traise, e1, Some e2, Some e3)
        (* Python2 only permits three of these at maximum. *)
        | _ -> invalid ());
      ]
  | `Pass_stmt tok ->
      let t = (* "pass" *) token env tok in
      [ Pass t ]
  | `Brk_stmt tok ->
      let t = (* "break" *) token env tok in
      [ Break t ]
  | `Cont_stmt tok ->
      let t = (* "continue" *) token env tok in
      [ Continue t ]
  | `Global_stmt (v1, v2, v3) ->
      let tglobal = (* "global" *) token env v1 in
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v2 in
      let ids =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 =
              (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v2
            in
            v2)
          v3
      in
      [ Global (tglobal, id :: ids) ]
  | `Nonl_stmt (v1, v2, v3) ->
      let tnonlocal = (* "nonlocal" *) token env v1 in
      let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v2 in
      let ids =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 =
              (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v2
            in
            v2)
          v3
      in
      [ NonLocal (tnonlocal, id :: ids) ]
  | `Exec_stmt (v1, v2, v3) ->
      let v1 = (* "exec" *) token env v1 in
      let v2 =
        match v2 with
        | `Str x ->
            let l, s, r = map_string_ env x in
            InterpolatedString (l, s, r)
        | `Id tok ->
            (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *)
            Literal (Str (str env tok))
      in
      let v3, v4 =
        match v3 with
        | Some (_v1TODO, v2, []) -> (Some (map_expression env v2), None)
        | Some (_v1TODO, v2, [ e ]) ->
            (Some (map_expression env v2), Some (map_expression env (e |> snd)))
        (* Python2 only permits two of these at maximum. *)
        | Some _ -> invalid ()
        | None -> (None, None)
      in
      [ Exec (v1, v2, v3, v4) ]
  | `Type_alias_stmt (v1, v2, v3, v4) ->
      let v1 = (* "type" *) token env v1 in
      let v2 = map_type_ env v2 in
      let _v3 = (* "=" *) token env v3 in
      let v4 = map_type_ env v4 in
      [ TypeAliasDef (v1, v2, v4) ]

let map_simple_statements (env : env) ((v1, v2, v3, v4) : CST.simple_statements)
    : stmt list =
  let v1 = map_simple_statement env v1 in
  let v2 =
    List_.map
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
  v1 :: v2 |> List_.flatten

let rec map_block (env : env) ((v1, v2) : CST.block) =
  let v1 = map_module_ env v1 in
  let _v2 = (* dedent *) token env v2 in
  v1

and map_case_clause (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.case_clause)
    : case_and_body =
  let v1 = (* "case" *) token env v1 in
  let p = map_case_pattern env v2 in
  let ps =
    List_.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        map_case_pattern env v2)
      v3
  in
  let v4 =
    match v4 with
    | Some _tok -> (* "," *) ()
    | None -> ()
  in
  let cond =
    match v5 with
    | Some x -> (
        match map_if_clause env x with
        | CompIf exp -> Some exp
        | CompFor _ -> raise Common.Impossible)
    | None -> None
  in
  let v6 = (* ":" *) token env v6 in
  let stmts = map_suite env v7 in
  CasesAndBody ([ Case (v1, PatTuple (fb (p :: ps))) ], stmts)

and map_class_definition (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.class_definition) : class_definition =
  let tclass = (* "class" *) token env v1 in
  let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v2 in
  let v3 =
    match v3 with
    | Some x -> Some (map_type_parameter env x)
    | None -> None
  in
  let parents =
    match v4 with
    | Some x ->
        let _l, xs, _r = map_argument_list env x in
        xs
    | None -> []
  in
  let _colon = (* ":" *) token env v5 in
  let body = map_suite env v6 in
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
      let cond = map_expression env v2 in
      let _tcolon = (* ":" *) token env v3 in
      let then_ = map_suite env v4 in
      let elseifs = List_.map (map_elif_clause env) v5 in
      let else_ =
        match v6 with
        | Some x -> Some (map_else_clause env x)
        | None -> None
      in
      let orelse =
        List_.fold_right
          (fun (tif, test, stmts) acc -> Some [ If (tif, test, stmts, acc) ])
          elseifs else_
      in
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
      let cond = map_expression env v2 in
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
            let excepts = List_.map (map_except_clause env) v1 in
            let else_opt = Option.map (map_try_else_clause env) v2 in
            let finally_opt = Option.map (map_finally_clause env) v3 in
            TryExcept (ttry, body, excepts, else_opt, finally_opt)
        | `Rep1_except_group_clause_opt_else_clause_opt_fina_clause (v1, v2, v3)
          ->
            let excepts = List_.map (map_except_group_clause env) v1 in
            let else_opt = Option.map (map_try_else_clause env) v2 in
            let finally_opt = Option.map (map_finally_clause env) v3 in
            TryExcept (ttry, body, excepts, else_opt, finally_opt)
        | `Fina_clause x ->
            let finally_opt = Some (map_finally_clause env x) in
            TryExcept (ttry, body, [], None, finally_opt)
      in
      res
  | `With_stmt (v1, v2, v3, v4, v5) -> (
      let _asyncTODO = map_async_opt env v1 in
      let twith = (* "with" *) token env v2 in
      let _tcolon = (* ":" *) token env v4 in
      let body = map_suite env v5 in
      match map_with_clause env v3 twith body with
      (* This shouldn't be produced outside of degenerate cases like `with ():`
         Unrealistic, but let's handle gracefully just in case.
      *)
      | None -> With (twith, (Tuple (CompList (fb []), no_ctx), None), body)
      | Some s -> s)
  | `Func_defi x ->
      let def = map_function_definition env x in
      FunctionDef def
  | `Class_defi x ->
      let def = map_class_definition env x in
      ClassDef def
  | `Deco_defi (v1, v2) ->
      let decorators = List_.map (map_decorator env) v1 in
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
  | `Match_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "match" *) token env v1 in
      let e = map_expression env v2 in
      let es =
        List_.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            map_expression env v2)
          v3
      in
      let () =
        match v4 with
        | Some _tok -> (* "," *) ()
        | None -> ()
      in
      let cond = Tuple (CompList (fb (e :: es)), no_ctx) in
      let v5 = (* ":" *) token env v5 in
      let cases = map_match_block env v6 in
      Switch (v1, cond, cases)

and map_elif_clause (env : env) ((v1, v2, v3, v4) : CST.elif_clause) =
  let v1 = (* "elif" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_suite env v4 in
  (v1, v2, v4)

and map_else_clause (env : env) ((v1, v2, v3) : CST.else_clause) =
  let _telse = (* "else" *) token env v1 in
  let _tcolon = (* ":" *) token env v2 in
  let body = map_suite env v3 in
  body

(* We only allow identifiers to appear in this position. *)
and id_opt_of_expr e =
  match e with
  | Name (id, _) -> Some id
  | _ -> None

and map_except_clause (env : env) ((v1, v2, v3, v4) : CST.except_clause) :
    excepthandler =
  let texpect = (* "except" *) token env v1 in
  let eopt, nameopt =
    match v2 with
    | Some (v1, v2) -> (
        let v1, aspat =
          match v1 with
          | `As_pat_ (v1, v2, v3) ->
              (* This site should be guarded, so it shouldn't be reached ideally.
                 As-patterns are not truly expressions, but they can occur in the context of a `with` or
                 `except`.
              *)
              let v1 = map_expression env v1 in
              let v2 = (* "as" *) token env v2 in
              let v3 = map_expression env v3 in
              (Some v1, id_opt_of_expr v3)
          | e -> (Some (map_expression env e), None)
        in
        match (v2, aspat) with
        | Some (t, v2), None ->
            let _t =
              match t with
              | `As tok -> (* "as" *) token env tok
              (* This is not legal in Python 3. There must be a tuple around this.
                  https://peps.python.org/pep-3110/#grammar-changes
              *)
              | `COMMA tok -> (* "," *) token env tok
            in
            let v2 = map_expression env v2 in
            (v1, id_opt_of_expr v2)
        (* It would be really weird for there to be this `as` after the other `as`.
           Let's just ignore one.
        *)
        | Some _, Some aspat -> (v1, Some aspat)
        | None, _ -> (v1, aspat))
    | None -> (None, None)
  in
  let _tcolon = (* ":" *) token env v3 in
  let body = map_suite env v4 in
  ExceptHandler (texpect, eopt, nameopt, body)

and map_except_group_clause (env : env)
    ((v1, v2, v3, v4, v5) : CST.except_group_clause) =
  let texcept = (* "except*" *) token env v1 in
  let e = map_expression env v2 in
  let nameopt =
    match v3 with
    | Some (v1, v2) ->
        let v1 = (* "as" *) token env v1 in
        let v2 = map_expression env v2 in
        id_opt_of_expr v2
    | None -> None
  in
  let v4 = (* ":" *) token env v4 in
  let body = map_suite env v5 in
  ExceptHandler (texcept, Some e, nameopt, body)

and map_try_else_clause (env : env) ((v1, v2, v3) : CST.finally_clause) =
  let telse = (* "else" *) token env v1 in
  let _tcolon = (* ":" *) token env v2 in
  let body = map_suite env v3 in
  (telse, body)

and map_finally_clause (env : env) ((v1, v2, v3) : CST.finally_clause) =
  let tfinally = (* "finally" *) token env v1 in
  let _tcolon = (* ":" *) token env v2 in
  let body = map_suite env v3 in
  (tfinally, body)

and map_function_definition (env : env)
    ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.function_definition) :
    function_definition =
  let _asyncTODO = map_async_opt env v1 in
  let tdef = (* "def" *) token env v2 in
  let id = (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env v3 in
  let v4 =
    match v4 with
    | Some x -> Some (map_type_parameter env x)
    | None -> None
  in
  let params = map_parameters env v5 in
  let topt =
    match v6 with
    | Some (v1, v2) ->
        let _v1 = (* "->" *) token env v1 in
        let v2 = map_type_ env v2 in
        Some v2
    | None -> None
  in
  let _tcolon = (* ":" *) token env v7 in
  let body = map_suite env v8 in
  (tdef, id, params, topt, body, [])

and map_match_block (env : env) (x : CST.match_block) =
  match x with
  | `Indent_rep_case_clause_dedent (v1, v2, v3) ->
      let v1 = (* indent *) token env v1 in
      let v2 = List_.map (map_case_clause env) v2 in
      let v3 = (* dedent *) token env v3 in
      v2
  | `Nl _tok ->
      (* newline *)
      []

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
    (fun () -> Tree_sitter_python.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      map_module_ env cst)

let parse_string ~file ~contents =
  H.wrap_parser
    (fun () -> Tree_sitter_python.Parse.string ~src_file:file contents)
    (fun cst _extras ->
      let env =
        {
          H.file = Fpath.v file;
          conv = (Pos.full_converters_str contents).linecol_to_bytepos_fun;
          extra = ();
        }
      in
      map_module_ env cst)

(* Need return type to be "any"*)
let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_python.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      Program (map_module_ env cst))
