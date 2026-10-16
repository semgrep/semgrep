(* Ruin0x11
 *
 * Copyright (c) 2020 Semgrep Inc.
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
module CST = Tree_sitter_lua.CST
module H = Parse_tree_sitter_helpers
module G = AST_generic
module H2 = AST_generic_helpers
module Log = Log_parser_lua.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* LUA parser using tree-sitter-lang/semgrep-lua and converting
 * directly to AST_generic
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-lua/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the lua CST
   to another type of tree.
*)

let mk_vars xs ys =
  let rec aux xs ys =
    match (xs, ys) with
    | [], [] -> []
    | x :: xs, [] -> (x, G.VarDef G.empty_var) :: aux xs ys
    | x :: xs, y :: ys ->
        (x, G.VarDef { G.vinit = Some y; vtype = None; vtok = G.no_sc })
        :: aux xs ys
    | [], _y :: _ys -> []
  in
  aux xs ys

let mk_assigns xs ys equal =
  let rec aux xs ys =
    match (xs, ys) with
    | [], [] -> []
    | _x :: _xs, [] -> []
    | x :: xs, y :: ys -> (G.Assign (x, equal, y) |> G.e) :: aux xs ys
    | [], _y :: _ys -> []
  in
  aux xs ys

let identifier (env : env) (tok : CST.identifier) : G.ident = str env tok

(* pattern [a-zA-Z_]\w* *)

let ident (env : env) (tok : CST.identifier) =
  G.Id (identifier env tok, G.empty_id_info ())

let string_literal env (x : CST.string_) =
  let escape = function
    | `Imm_tok_bslash_choice_pat_4504a10 t
    | `Imm_tok_bslash_choice_cr t ->
        str env t
  in
  let lp, contents, rp =
    match x with
    | `Blk_str (lp, content, rp) -> (lp, [ str env content ], rp)
    | `Quote_str (`DQUOT_opt_doub_str_content_DQUOT (lp, contents, rp)) ->
        let contents =
          Option.value ~default:[] contents
          |> List.map (function
            | `Imm_tok_prec_p1_pat_3a2a380 t -> str env t
            | `Esc_seq x -> escape x)
        in
        (lp, contents, rp)
    | `Quote_str (`SQUOT_opt_sing_str_content_SQUOT (lp, contents, rp)) ->
        let contents =
          Option.value ~default:[] contents
          |> List.map (function
            | `Imm_tok_prec_p1_pat_dc28280 t -> str env t
            | `Esc_seq x -> escape x)
        in
        (lp, contents, rp)
  in
  let lp = token env lp and rp = token env rp in
  let text = String.concat "" (List.map fst contents) in
  let tok =
    match List.map snd contents with
    | [] -> lp
    | first :: rest -> Tok.combine_toks first rest
  in
  G.L (G.String (lp, (text, tok), rp)) |> G.e

let number_literal env tok =
  let text, tok = str env tok in
  let lower = String.lowercase_ascii text in
  let without n = Str.first_chars text (String.length text - n) in
  if String.ends_with ~suffix:"ull" lower then
    G.Int (Parsed_int.parse (without 3, tok))
  else if String.ends_with ~suffix:"ll" lower then
    G.Int (Parsed_int.parse (without 2, tok))
  else if String.ends_with ~suffix:"i" lower then G.Imag (without 1, tok)
  else if String.starts_with ~prefix:"0b" lower then
    G.Float (Parsed_int.to_float_opt (Parsed_int.parse (lower, tok)), tok)
  else G.Float (float_of_string_opt text, tok)

let map_field_sep (env : env) (x : CST.field_sep) =
  match x with
  | `COMMA tok -> token env tok (* "," *)
  | `SEMI tok -> token env tok

(* ";" *)

(* let map_number (env : env) (tok : CST.number) =
 *   token env tok (\* number *\) *)

(* let map_identifier (env : env) (tok : CST.identifier) =
 *   token env tok (\* pattern [a-zA-Z_][a-zA-Z0-9_]* *\) *)

(* let map_string_ (env : env) (tok : CST.string_) =
 *   token env tok (\* string *\) *)

let map_vararg_parameter env (dots, name) =
  match name with
  | None -> G.ParamEllipsis (token env dots)
  | Some name ->
      G.ParamRest (token env dots, G.param_of_id (identifier env name))

let map_parameters env ((lp, params, rp) : CST.parameters) =
  let param id = G.Param (G.param_of_id (identifier env id)) in
  let tail = function
    | None -> []
    | Some (_, rest) -> [ map_vararg_parameter env rest ]
  in
  let params =
    match params with
    | None -> []
    | Some
        (`Choice_id_rep_COMMA_id_opt_COMMA_vararg_param
           (`Id_rep_COMMA_id_opt_COMMA_vararg_param (first, rest, vararg))) ->
        (param first :: List.map (fun (_, id) -> param id) rest) @ tail vararg
    | Some (`Choice_id_rep_COMMA_id_opt_COMMA_vararg_param (`Vararg_param x)) ->
        [ map_vararg_parameter env x ]
    | Some
        (`Vararg_exp_rep1_COMMA_id_opt_COMMA_vararg_param (dots, rest, vararg))
      ->
        G.ParamEllipsis (token env dots)
        :: List.map (fun (_, id) -> param id) rest
        @ tail vararg
    | Some (`Vararg_exp_COMMA_vararg_param (dots, _, vararg)) ->
        [ G.ParamEllipsis (token env dots); map_vararg_parameter env vararg ]
  in
  (token env lp, params, token env rp)

let rec map_function_name_prefix env = function
  | `Id id -> [ identifier env id ]
  | `Func_name_dot_index_exp (prefix, _, id) ->
      map_function_name_prefix env prefix @ [ identifier env id ]

let map_function_name env = function
  | `Func_name_prefix_exp x -> H2.name_of_ids (map_function_name_prefix env x)
  | `Func_name_meth_index_exp (prefix, colon, id) ->
      (* QExpr keeps ':' distinct from a dotted function name. *)
      let receiver = H2.name_of_ids (map_function_name_prefix env prefix) in
      G.IdQualified
        {
          G.name_last = (identifier env id, None);
          name_middle = Some (G.QExpr (G.N receiver |> G.e, token env colon));
          name_top = None;
          name_info = G.empty_id_info ();
        }

let rec map_expression_list (env : env) ((v1, v2) : CST.expression_list) :
    G.expr list =
  let v1 = map_expression env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* "," *)
        in
        let v2 = map_expression env v2 in
        v2)
      v2
  in
  v1 :: v2

and map_expression_tuple (env : env) ((v1, v2) : CST.expression_list) : G.expr =
  let v1 = map_expression_list env (v1, v2) in
  G.Container (G.Tuple, Tok.unsafe_fake_bracket v1) |> G.e

and map_anon_arguments (env : env) ((v1, v2) : CST.expression_list) :
    G.argument list =
  let v1 = map_expression_list env (v1, v2) in
  List.map (fun (v1 : G.expr) -> G.Arg v1) v1

and map_arguments (env : env) (x : CST.arguments) : G.arguments =
  match x with
  | `LPAR_opt_exp_rep_COMMA_exp_RPAR (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "(" *)
      in
      let v2 =
        match v2 with
        | Some x -> map_anon_arguments env x
        | None -> []
      in
      let v3 =
        token env v3
        (* ")" *)
      in
      (v1, v2, v3)
  | `Table_cons x -> fb [ G.Arg (map_table env x) ]
  | `Str tok -> fb [ G.Arg (string_literal env tok) ]

(* string *)
and map_binary_operation (env : env) (x : CST.binary_expression) =
  match x with
  | `Exp_or_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* "or" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.Or, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_and_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* "and" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.And, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* "<" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.Lt, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* "<=" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.LtE, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* "==" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.Eq, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_TILDEEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* "~=" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.NotEq, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* ">=" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.GtE, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* ">" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.Gt, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* "|" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.BitOr, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_TILDE_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* "~" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.BitNot, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* "&" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.BitAnd, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* "<<" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.LSL, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* ">>" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.LSR, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* "+" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.Plus, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* "-" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.Minus, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* "*" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.Mult, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* "/" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.Div, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_SLASHSLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* "//" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.FloorDiv, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* "%" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.Mod, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_DOTDOT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* ".." *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.Concat, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        token env v2
        (* "^" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.Special (G.Op G.BitXor, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])

and map_statement_list (env : env) (x : CST.statement list) : G.stmt list =
  let v1 = List.map (map_statement env) x in
  List_.flatten v1

and map_statements_and_return (env : env) (v1, v2) : G.stmt list =
  let v1 = map_statement_list env v1 in
  let v3 =
    match v2 with
    | Some x ->
        let v4 = map_return_statement env x in
        List.append v1 [ v4 ]
    | None -> v1
  in
  v3

and map_block env = function
  | None -> []
  | Some (`Rep1_stmt_opt_ret_stmt (stmts, ret)) ->
      map_statements_and_return env (stmts, ret)
  | Some (`Rep_stmt_ret_stmt (stmts, ret)) ->
      map_statements_and_return env (stmts, Some ret)

and map_do_block env (start, body, finish) =
  G.Block (token env start, map_block env body, token env finish) |> G.s

and map_else_ env ((_, body) : CST.else_statement) =
  G.Block (fb (map_block env body)) |> G.s

and map_expression (env : env) (x : CST.expression) : G.expr =
  (match x with
    | `Vararg_exp tok -> G.Ellipsis (token env tok) (* "..." *)
    | `Var x -> (map_variable env x).G.e
    | `Func_call x -> (map_function_call_expr env x).G.e
    | `Paren_exp (_, x, _) -> (map_expression env x).G.e
    | `Func_defi (v1, v2) ->
        let _t =
          token env v1
          (* "function" *)
        in
        let v2 = map_function_body env v2 v1 in
        G.Lambda v2
    | `Table_cons x ->
        let x = map_table env x in
        x.G.e
    | `Bin_exp x -> map_binary_operation env x
    | `Un_exp (v1, v2) ->
        let op, tok =
          match v1 with
          | `Not tok -> (G.Plus, tok) (* "not" *)
          | `HASH tok -> (G.Length, tok) (* "#" *)
          | `DASH tok -> (G.Minus, tok) (* "-" *)
          | `TILDE tok -> (G.BitNot, tok)
          (* "~" *)
        in
        let v2 = map_expression env v2 in
        G.Call (G.Special (G.Op op, token env tok) |> G.e, fb [ G.Arg v2 ])
    | `Str tok ->
        let x =
          string_literal env tok
          (* string *)
        in
        x.G.e
    | `Num tok -> G.L (number_literal env tok)
    | `Nil tok -> G.L (G.Null (token env tok)) (* "nil" *)
    | `True tok -> G.L (G.Bool (true, token env tok)) (* "true" *)
    | `False tok -> G.L (G.Bool (false, token env tok)) (* "false" *))
  |> G.e

(* pattern [a-zA-Z_][a-zA-Z0-9_]* *)
and map_field (env : env) (x : CST.field) : G.expr =
  let ent, tok, def =
    match x with
    | `LBRACK_exp_RBRACK_EQ_exp (v1, v2, v3, v4, v5) ->
        let _v1 =
          token env v1
          (* "[" *)
        in
        let v2 = map_expression env v2 in
        let v3 =
          token env v3
          (* "]" *)
        in
        let _v4 =
          token env v4
          (* "=" *)
        in
        let v5 = map_expression env v5 in
        (v2, v3, v5)
    | `Choice_cont_kw_EQ_exp ((`Id v1 | `Cont_kw v1), v2, v3) ->
        let v1 =
          identifier env v1
          (* pattern [a-zA-Z_][a-zA-Z0-9_]* *)
        in
        let v2 =
          token env v2
          (* "=" *)
        in
        let v3 = map_expression env v3 in
        (G.N (G.Id (v1, G.empty_id_info ())) |> G.e, v2, v3)
    | `Exp x ->
        let expr = map_expression env x in
        let ident =
          G.Special (G.NextArrayIndex, G.fake "next_array_index") |> G.e
        in
        (ident, G.fake "=", expr)
  in
  G.Assign (ent, tok, def) |> G.e

and map_field_sequence (env : env) ((v1, v2, v3) : CST.field_list) : G.expr list
    =
  let v1 = map_field env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = map_field_sep env v1 in
        let v2 = map_field env v2 in
        v2)
      v2
  in
  let _v3 =
    match v3 with
    | Some x -> [ map_field_sep env x ]
    | None -> []
  in
  v1 :: v2

and map_function_body (env : env) ((v1, v2, v4) : CST.function_body)
    (name : CST.identifier) : G.function_definition =
  let v1 = map_parameters env v1 in
  let body = map_do_block env (name, v2, v4) in
  {
    G.fparams = v1;
    frettype = None;
    fkind = (G.Function, token env name);
    fbody = G.FBStmt body;
  }

and map_qualified env prefix sep id =
  G.N
    (G.IdQualified
       {
         G.name_last = (identifier env id, None);
         name_middle = Some (G.QExpr (map_prefix env prefix, token env sep));
         name_top = None;
         name_info = G.empty_id_info ();
       })
  |> G.e

and map_function_call_expr env ((callee, args) : CST.function_call) =
  let callee =
    match callee with
    | `Prefix_exp x -> map_prefix env x
    | `Meth_index_exp (prefix, colon, id) -> map_qualified env prefix colon id
  in
  G.Call (callee, map_arguments env args) |> G.e

and map_in_loop_expression (env : env)
    (((v1, v2), v3, (v4, v5)) : CST.for_generic_clause) =
  let v1 =
    identifier env v1
    (* pattern [a-zA-Z_][a-zA-Z0-9_]* *)
  in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* "," *)
        in
        let v2 =
          identifier env v2
          (* pattern [a-zA-Z_][a-zA-Z0-9_]* *)
        in
        v2)
      v2
  in
  let pat =
    match v2 with
    | [] -> G.PatId (v1, G.empty_id_info ()) |> G.p
    | _ ->
        let pats =
          List.map (fun id -> G.PatId (id, G.empty_id_info ())) (v1 :: v2)
        in
        G.PatTuple (fb (pats |> G.p))
  in
  (* TODO *)
  let v3 =
    token env v3
    (* "in" *)
  in
  (* Lua has really weird semantics for if there are multiple things in a
     generic for.
     https://www.lua.org/pil/7.2.html
     Basically, the list of expressions just needs to be unpackable into
     three things (which is automatically done for simple iterators).
     These are the folding function, the accumulator, and initial value.
     It's not really something that would be easy to represent in the
     Generic AST. For most purposes, this is usually just a single iterator,
     and the translation in the case where it actually is multiple things
     would be quite complicated.
     Let's just not deal with it for now.
  *)
  let v4 = map_expression env v4 in
  let _v5_TODO =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* "," *)
        in
        let v2 = map_expression env v2 in
        v2)
      v5
  in
  G.ForEach (pat, v3, v4)

and map_loop_expression (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.for_numeric_clause) =
  let v1 =
    identifier env v1
    (* pattern [a-zA-Z_][a-zA-Z0-9_]* *)
  in
  let _v2 =
    token env v2
    (* "=" *)
  in
  let _v3 = map_expression env v3 in
  let var : G.variable_definition = G.empty_var in
  let for_init_var = G.ForInitVar (G.basic_entity v1, var) in
  let _v4 =
    token env v4
    (* "," *)
  in
  let v5 = map_expression env v5 in
  let v6 =
    match v6 with
    | Some (v1, v2) ->
        let _v1 =
          token env v1
          (* "," *)
        in
        let v2 = map_expression env v2 in
        Some v2
    | None -> None
  in
  G.ForClassic ([ for_init_var ], Some v5, v6)

and map_prefix env (x : CST.prefix_expression) =
  match x with
  | `Var x -> map_variable env x
  | `Func_call x -> map_function_call_expr env x
  | `Paren_exp (_, x, _) -> map_expression env x

and map_return_statement (env : env) ((v1, v2, v3) : CST.return_statement) =
  let v1 =
    token env v1
    (* "return" *)
  in
  let v2 =
    match v2 with
    | Some x -> Some (map_expression_tuple env x)
    | None -> None
  in
  let v3 =
    match v3 with
    | Some tok -> token env tok (* ";" *)
    | None -> G.sc
  in
  G.Return (v1, v2, v3) |> G.s

and map_declaration env (x : CST.declaration) =
  let function_def attrs tok name body =
    let ent = { G.name = G.EN name; attrs; tparams = None } in
    [ G.DefStmt (ent, G.FuncDef (map_function_body env body tok)) |> G.s ]
  in
  match x with
  | `Func_decl (tok, name, body) ->
      function_def [] tok (map_function_name env name) body
  | `Local_func_decl (local, tok, id, body) ->
      function_def
        [ G.KeywordAttr (G.Static, token env local) ]
        tok
        (H2.name_of_id (identifier env id))
        body
  | `Global_func_decl (global, tok, id, body) ->
      function_def
        [ G.unhandled_keywordattr (str env global) ]
        tok
        (H2.name_of_id (identifier env id))
        body
  | `Var_decl (local, decl) ->
      map_variable_declaration env
        [ G.KeywordAttr (G.Static, token env local) ]
        decl
  | `Global_var_decl (global, decl) ->
      map_variable_declaration env
        [ G.unhandled_keywordattr (str env global) ]
        decl
  | `Global_impl_var_decl (global, attr, star) ->
      (* Lua's wildcard global declaration changes name resolution, not a
         particular variable. There is no equivalent Generic directive. *)
      [
        G.DirectiveStmt
          {
            G.d =
              G.OtherDirective
                (("global", token env global), [ G.Tk (token env star) ]);
            d_attrs = map_attribute env attr;
          }
        |> G.s;
      ]

and map_attribute env = function
  | None -> []
  | Some (lt, id, gt) ->
      let name = identifier env id in
      [
        (match name with
        | "const", tok -> G.KeywordAttr (G.Const, tok)
        | _ ->
            G.NamedAttr
              ( token env lt,
                H2.name_of_id name,
                (token env lt, [], token env gt) ));
      ]

and map_variable_declaration env attrs decl =
  let names, exprs =
    match decl with
    | `Att_name_list names -> (names, [])
    | `Var_assign (names, _, values) -> (names, map_expression_list env values)
  in
  let leading, first, attr, rest = names in
  let common = attrs @ map_attribute env leading in
  let entity id attr =
    G.basic_entity (identifier env id) ~attrs:(common @ map_attribute env attr)
  in
  let entities =
    entity first attr :: List.map (fun (_, id, attr) -> entity id attr) rest
  in
  List.map (fun def -> G.DefStmt def |> G.s) (mk_vars entities exprs)

and map_statement env (x : CST.statement) =
  match x with
  | `Exp x -> [ G.ExprStmt (map_expression env x, G.sc) |> G.s ]
  | `Choice_empty_stmt x -> (
      match x with
      | `Assign_stmt ((first, rest), eq, values) ->
          let lhs = List.map (map_variable env) (first :: List.map snd rest) in
          let rhs = map_expression_list env values in
          List.map
            (fun x -> G.ExprStmt (x, G.sc) |> G.s)
            (mk_assigns lhs rhs (token env eq))
      | `Decl x -> map_declaration env x
      | `Do_stmt x -> [ map_do_block env x ]
      | `If_stmt (tok, cond, _, body, elseifs, otherwise, _) ->
          let otherwise = Option.map (map_else_ env) otherwise in
          let otherwise =
            List_.fold_right
              (fun (tok, cond, _, body) acc ->
                Some
                  (G.If
                     ( token env tok,
                       G.Cond (map_expression env cond),
                       G.Block (fb (map_block env body)) |> G.s,
                       acc )
                  |> G.s))
              elseifs otherwise
          in
          [
            G.If
              ( token env tok,
                G.Cond (map_expression env cond),
                G.Block (fb (map_block env body)) |> G.s,
                otherwise )
            |> G.s;
          ]
      | `While_stmt (tok, cond, start, body, finish) ->
          [
            G.While
              ( token env tok,
                G.Cond (map_expression env cond),
                map_do_block env (start, body, finish) )
            |> G.s;
          ]
      | `Repeat_stmt (tok, body, until, cond) ->
          [
            G.DoWhile
              ( token env tok,
                map_do_block env (tok, body, until),
                map_expression env cond )
            |> G.s;
          ]
      | `For_stmt (tok, clause, start, body, finish) ->
          let clause =
            match clause with
            | `For_gene_clause x -> map_in_loop_expression env x
            | `For_nume_clause x -> map_loop_expression env x
          in
          [
            G.For (token env tok, clause, map_do_block env (start, body, finish))
            |> G.s;
          ]
      | `Goto_stmt (tok, id) ->
          [ G.Goto (token env tok, identifier env id, G.sc) |> G.s ]
      | `Brk_stmt tok -> [ G.Break (token env tok, G.LNone, G.sc) |> G.s ]
      | `Label_stmt (_, id, _) ->
          [ G.Label (identifier env id, G.Block (fb []) |> G.s) |> G.s ]
      | `Empty_stmt _ -> []
      | `Func_call x ->
          [ G.ExprStmt (map_function_call_expr env x, G.sc) |> G.s ])

and map_table (env : env) ((v1, v2, v3) : CST.table_constructor) : G.expr =
  let v1 =
    token env v1
    (* "{" *)
  in
  let v2 =
    match v2 with
    | Some x -> map_field_sequence env x
    | None -> []
  in
  let v3 =
    token env v3
    (* "}" *)
  in
  G.Container (G.Dict, (v1, v2, v3)) |> G.e

and map_variable env (x : CST.variable) =
  match x with
  | `Id id
  | `Cont_kw id ->
      G.N (ident env id) |> G.e
  | `Brac_index_exp (prefix, lp, index, rp) ->
      G.ArrayAccess
        ( map_prefix env prefix,
          (token env lp, map_expression env index, token env rp) )
      |> G.e
  | `Dot_index_exp (prefix, dot, id) -> map_qualified env prefix dot id

let map_program env ((_, stmts, ret) : CST.chunk) =
  map_statements_and_return env (stmts, ret)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_lua.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in

      map_program env cst)

(* todo: special mode to convert Ellipsis in the right construct! *)
let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_lua.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      let xs = map_program env cst in
      G.Ss xs)
