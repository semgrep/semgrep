(* Yoann Padioleau
 *
 * Copyright (c) 2022 R2C
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
module CST = Tree_sitter_r.CST
module H = Parse_tree_sitter_helpers
open AST_generic
module G = AST_generic
module H2 = AST_generic_helpers

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket

(* Convert the R CST directly to Generic AST. Newline and delimiter tokens
   determine CST structure but do not introduce additional semantic nodes. *)
let map_identifier (env : env) tok =
  let s, t = str env tok in
  let n = String.length s in
  if n >= 2 && s.[0] = '`' then (String.sub s 1 (n - 2), t) else (s, t)

let map_name env = function
  | `Id tok -> map_identifier env tok
  | `Dots tok
  | `Dot_dot_i tok ->
      str env tok

let map_float env = function
  | `Hex_lit tok
  | `Num_lit tok ->
      str env tok

let map_na env = function
  | `NA tok
  | `NA_char_ tok
  | `NA_comp_ tok
  | `NA_int_ tok
  | `NA_real_ tok ->
      str env tok

let map_string_ env x =
  let l, xs, r =
    match x with
    | `Raw_str (l, content, r) ->
        (l, Option.to_list content |> List.map (str env), r)
    | `Single_quoted_str (l, content, r) ->
        ( l,
          Option.value ~default:[] content
          |> List.map (function
              | `Pat_dc28280 tok
              | `Esc_seq tok
              -> str env tok),
          r )
    | `Double_quoted_str (l, content, r) ->
        ( l,
          Option.value ~default:[] content
          |> List.map (function
              | `Pat_3a2a380 tok
              | `Esc_seq tok
              -> str env tok),
          r )
  in
  G.string_ (token env l, xs, token env r)

let map_string_or_identifier env = function
  | `Str x -> Tok.unbracket (map_string_ env x)
  | `Choice_dots x -> map_name env x

let missing_argument tok = OtherArg (("Missing argument", tok), [])

let rec map_argument env = function
  | `Arg_unna x -> Arg (map_expression env x)
  | `Arg_named (name, eq, value) -> (
      let id =
        match name with
        | `Choice_str x -> map_string_or_identifier env x
        | `Null tok -> str env tok
      in
      match value with
      | Some x -> ArgKwd (id, map_expression env x)
      | None -> OtherArg (("Empty =", token env eq), [ I id ]))

and map_arguments env (l, first, rest, r) =
  (* Missing arguments are positional, including a final missing argument.
     An empty argument list, unlike f(,), has no missing argument. *)
  let missing tok = missing_argument (token env tok) in
  let xs =
    match (first, rest) with
    | None, [] -> []
    | _ ->
        (match first with
        | Some x -> map_argument env x
        | None -> missing l)
        :: List.map
             (fun (comma, x) ->
               match x with
               | Some x -> map_argument env x
               | None -> missing comma)
             rest
  in
  (token env l, xs, token env r)

and map_subscript_arguments env args =
  let l, xs, r = map_arguments env args in
  let xs =
    match xs with
    | [] -> [ missing_argument l ]
    | _ -> xs
  in
  (l, xs, r)

and map_expression env (x : CST.expression) =
  match x with
  | `Id tok -> N (H2.name_of_id (map_identifier env tok)) |> G.e
  | `Dot_dot_i tok
  | `Inf tok
  | `Nan tok ->
      N (H2.name_of_id (str env tok)) |> G.e
  | `Na x -> N (H2.name_of_id (map_na env x)) |> G.e
  | `Dots tok -> Ellipsis (token env tok) |> G.e
  | `Float x ->
      let s, t = map_float env x in
      L (Float (Float.of_string_opt s, t)) |> G.e
  | `Int (x, suffix) -> (
      let s, t = map_float env x in
      let t = Tok.combine_toks t [ token env suffix ] in
      (* R accepts decimal and exponent notation before L. Non-integral or
         out-of-range values remain doubles, as in R's numeric constants. *)
      let value = Float.of_string_opt s in
      match value with
      | Some f when f >= 0. && f <= 2147483647. && Float.floor f = f ->
          L (Int (Some (Int64.of_float f), t)) |> G.e
      | _ -> L (Float (value, t)) |> G.e)
  | `Comp (x, suffix) ->
      let s, t = map_float env x in
      L (Imag (s, Tok.combine_toks t [ token env suffix ])) |> G.e
  | `Str x -> L (String (map_string_ env x)) |> G.e
  | `True tok -> L (Bool (true, token env tok)) |> G.e
  | `False tok -> L (Bool (false, token env tok)) |> G.e
  | `Null tok -> L (Null (token env tok)) |> G.e
  | `Call (f, args) ->
      Call (map_expression env f, map_arguments env args) |> G.e
  | `Subset (base, args) -> (
      let base = map_expression env base in
      let l, xs, r = map_subscript_arguments env args in
      match xs with
      | [ Arg index ] -> ArrayAccess (base, (l, index, r)) |> G.e
      | xs -> OtherExpr (("ArrayAccess[xs]", l), [ E base; Args xs ]) |> G.e)
  | `Subset2 (base, args) ->
      let l, xs, _r = map_subscript_arguments env args in
      OtherExpr
        (("ArrayAccess[[xs]]", l), [ E (map_expression env base); Args xs ])
      |> G.e
  | `Paren_exp (_l, x, _r) -> map_expression env x
  | `Func_defi (kind, _nl, params, _nl2, body) ->
      let tok =
        match kind with
        | `Func tok
        | `BSLASH tok ->
            token env tok
      in
      Lambda
        {
          fkind = (LambdaKind, tok);
          fparams = map_parameters env params;
          frettype = None;
          fbody = FBExpr (map_expression env body);
        }
      |> G.e
  | `Bin_op x -> map_binary env x
  | `Un_op x -> map_unary env x
  | `Extr_op x -> map_extract env x
  | `Name_op x -> map_namespace env x
  | `Braced_exp _
  | `If_stmt _
  | `For_stmt _
  | `While_stmt _
  | `Repeat_stmt _
  | `Brk _
  | `Next _ ->
      G.stmt_to_expr (map_statement env x)

and map_statement env = function
  | `Braced_exp (l, xs, r) ->
      Block (token env l, map_statements env xs, token env r) |> G.s
  | `If_stmt (tok, _nl, _l, cond, _r, _nl2, yes, no) ->
      If
        ( token env tok,
          Cond (map_expression env cond),
          map_statement env yes,
          Option.map (fun (_else, _nl, x) -> map_statement env x) no )
      |> G.s
  | `For_stmt (tok, _nl, _l, name, in_, seq, _r, _nl2, body) ->
      For
        ( token env tok,
          ForEach
            ( PatId (map_name env name, G.empty_id_info ()),
              token env in_,
              map_expression env seq ),
          map_statement env body )
      |> G.s
  | `While_stmt (tok, _nl, _l, cond, _r, _nl2, body) ->
      While
        (token env tok, Cond (map_expression env cond), map_statement env body)
      |> G.s
  | `Repeat_stmt (tok, _nl, body) ->
      let tok = token env tok in
      While (tok, OtherCond (("Repeat", tok), []), map_statement env body)
      |> G.s
  | `Brk tok -> Break (token env tok, LNone, G.sc) |> G.s
  | `Next tok -> Continue (token env tok, LNone, G.sc) |> G.s
  | x -> G.exprstmt (map_expression env x)

and map_statements env xs =
  List.filter_map
    (function
      | `Exp x -> Some (map_statement env x)
      | `Semi _
      | `Nl _ ->
          None)
    xs

and map_parameter env = function
  | `Param_with_defa_6e24c8f (`Dots tok) -> ParamEllipsis (token env tok)
  | `Param_with_defa_6e24c8f name -> Param (G.param_of_id (map_name env name))
  | `Param_with_defa_d9d11f1 (name, _eq, value) ->
      Param
        (G.param_of_id (map_name env name) ~pdefault:(map_expression env value))

and map_parameters env (l, params, r) =
  let xs =
    match params with
    | None -> []
    | Some (first, rest) ->
        map_parameter env first
        :: List.map (fun (_, x) -> map_parameter env x) rest
  in
  (token env l, xs, token env r)

and map_extract env x =
  let base, op, rhs, is_slot =
    match x with
    | `Exp_DOLLAR_rep_nl_opt_choice_str (base, op, _nl, rhs) ->
        (base, op, rhs, false)
    | `Exp_AT_rep_nl_opt_choice_str (base, op, _nl, rhs) -> (base, op, rhs, true)
  in
  let base = map_expression env base in
  let op = token env op in
  match rhs with
  | Some rhs ->
      let id = map_string_or_identifier env rhs in
      if is_slot then OtherExpr (("@", op), [ E base; I id ]) |> G.e
      else DotAccess (base, op, FN (H2.name_of_id id)) |> G.e
  | None ->
      (* Upstream represents incomplete extraction explicitly. *)
      OtherExpr (("Incomplete extraction", op), [ E base ]) |> G.e

and map_namespace env x =
  let lhs, op, rhs, internal =
    match x with
    | `Choice_str_COLONCOLON_opt_choice_str (lhs, op, rhs) ->
        (lhs, op, rhs, false)
    | `Choice_str_COLONCOLONCOLON_opt_choice_str (lhs, op, rhs) ->
        (lhs, op, rhs, true)
  in
  let lhs = map_string_or_identifier env lhs in
  match rhs with
  | Some rhs ->
      let rhs = map_string_or_identifier env rhs in
      if internal then
        OtherExpr ((":::", token env op), [ I lhs; I rhs ]) |> G.e
      else N (H2.name_of_ids [ lhs; rhs ]) |> G.e
  | None -> OtherExpr (("Incomplete namespace", token env op), [ I lhs ]) |> G.e

and map_binary env (x : CST.binary_operator) =
  match x with
  | `Exp_PLUS_rep_nl_exp (lhs, op, _nl, rhs) ->
      G.opcall
        (Plus, token env op)
        [ map_expression env lhs; map_expression env rhs ]
  | `Exp_DASH_rep_nl_exp (lhs, op, _nl, rhs) ->
      G.opcall
        (Minus, token env op)
        [ map_expression env lhs; map_expression env rhs ]
  | `Exp_STAR_rep_nl_exp (lhs, op, _nl, rhs) ->
      G.opcall
        (Mult, token env op)
        [ map_expression env lhs; map_expression env rhs ]
  | `Exp_SLASH_rep_nl_exp (lhs, op, _nl, rhs) ->
      G.opcall
        (Div, token env op)
        [ map_expression env lhs; map_expression env rhs ]
  | `Exp_STARSTAR_rep_nl_exp (lhs, op, _nl, rhs) ->
      G.opcall
        (Pow, token env op)
        [ map_expression env lhs; map_expression env rhs ]
  | `Exp_HAT_rep_nl_exp (lhs, op, _nl, rhs) ->
      G.opcall
        (Pow, token env op)
        [ map_expression env lhs; map_expression env rhs ]
  | `Exp_LT_rep_nl_exp (lhs, op, _nl, rhs) ->
      G.opcall
        (Lt, token env op)
        [ map_expression env lhs; map_expression env rhs ]
  | `Exp_GT_rep_nl_exp (lhs, op, _nl, rhs) ->
      G.opcall
        (Gt, token env op)
        [ map_expression env lhs; map_expression env rhs ]
  | `Exp_LTEQ_rep_nl_exp (lhs, op, _nl, rhs) ->
      G.opcall
        (LtE, token env op)
        [ map_expression env lhs; map_expression env rhs ]
  | `Exp_GTEQ_rep_nl_exp (lhs, op, _nl, rhs) ->
      G.opcall
        (GtE, token env op)
        [ map_expression env lhs; map_expression env rhs ]
  | `Exp_EQEQ_rep_nl_exp (lhs, op, _nl, rhs) ->
      G.opcall
        (PhysEq, token env op)
        [ map_expression env lhs; map_expression env rhs ]
  | `Exp_BANGEQ_rep_nl_exp (lhs, op, _nl, rhs) ->
      G.opcall
        (NotEq, token env op)
        [ map_expression env lhs; map_expression env rhs ]
  | `Exp_BAR_rep_nl_exp (lhs, op, _nl, rhs) ->
      G.opcall
        (BitOr, token env op)
        [ map_expression env lhs; map_expression env rhs ]
  | `Exp_AMP_rep_nl_exp (lhs, op, _nl, rhs) ->
      G.opcall
        (BitAnd, token env op)
        [ map_expression env lhs; map_expression env rhs ]
  | `Exp_BARBAR_rep_nl_exp (lhs, op, _nl, rhs) ->
      G.opcall
        (Or, token env op)
        [ map_expression env lhs; map_expression env rhs ]
  | `Exp_AMPAMP_rep_nl_exp (lhs, op, _nl, rhs) ->
      G.opcall
        (And, token env op)
        [ map_expression env lhs; map_expression env rhs ]
  | `Exp_EQ_rep_nl_exp (lhs, op, _nl, rhs) ->
      G.opcall
        (Eq, token env op)
        [ map_expression env lhs; map_expression env rhs ]
  | `Exp_LTDASH_rep_nl_exp (lhs, op, _nl, rhs) ->
      Assign (map_expression env lhs, token env op, map_expression env rhs)
      |> G.e
  | `Exp_LTLTDASH_rep_nl_exp (lhs, op, _nl, rhs) ->
      OtherExpr
        ( ("<<=", token env op),
          [ E (map_expression env lhs); E (map_expression env rhs) ] )
      |> G.e
  | `Exp_COLONEQ_rep_nl_exp (lhs, op, _nl, rhs) ->
      OtherExpr
        ( (":=", token env op),
          [ E (map_expression env lhs); E (map_expression env rhs) ] )
      |> G.e
  | `Exp_DASHGT_rep_nl_exp (lhs, op, _nl, rhs) ->
      OtherExpr
        ( ("->", token env op),
          [ E (map_expression env lhs); E (map_expression env rhs) ] )
      |> G.e
  | `Exp_DASHGTGT_rep_nl_exp (lhs, op, _nl, rhs) ->
      OtherExpr
        ( ("->>", token env op),
          [ E (map_expression env lhs); E (map_expression env rhs) ] )
      |> G.e
  | `Exp_BARGT_rep_nl_exp (lhs, op, _nl, rhs) ->
      OtherExpr
        ( ("|>", token env op),
          [ E (map_expression env lhs); E (map_expression env rhs) ] )
      |> G.e
  | `Exp_COLON_rep_nl_exp (lhs, op, _nl, rhs) ->
      OtherExpr
        ( (":", token env op),
          [ E (map_expression env lhs); E (map_expression env rhs) ] )
      |> G.e
  | `Exp_TILDE_rep_nl_exp (lhs, op, _nl, rhs) ->
      OtherExpr
        ( ("~", token env op),
          [ E (map_expression env lhs); E (map_expression env rhs) ] )
      |> G.e
  | `Exp_QMARK_rep_nl_exp (lhs, op, _nl, rhs) ->
      OtherExpr
        ( ("?", token env op),
          [ E (map_expression env lhs); E (map_expression env rhs) ] )
      |> G.e
  | `Exp_pat_43ed24e_rep_nl_exp (lhs, op, _nl, rhs) ->
      Call
        ( N (H2.name_of_id (str env op)) |> G.e,
          fb [ Arg (map_expression env lhs); Arg (map_expression env rhs) ] )
      |> G.e

and map_unary env (x : CST.unary_operator) =
  match x with
  | `PLUS_rep_nl_exp (op, _nl, rhs) ->
      G.opcall (Plus, token env op) [ map_expression env rhs ]
  | `DASH_rep_nl_exp (op, _nl, rhs) ->
      G.opcall (Minus, token env op) [ map_expression env rhs ]
  | `BANG_rep_nl_exp (op, _nl, rhs) ->
      G.opcall (Not, token env op) [ map_expression env rhs ]
  | `TILDE_rep_nl_exp (op, _nl, rhs) ->
      G.opcall (BitNot, token env op) [ map_expression env rhs ]
  | `QMARK_rep_nl_exp (op, _nl, rhs) ->
      OtherExpr (("?", token env op), [ E (map_expression env rhs) ]) |> G.e

let map_program env (_start, xs) = map_statements env xs

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_r.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      map_program env cst)

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_r.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      G.Ss (map_program env cst))
