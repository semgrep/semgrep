(* Ruin0x11
 *
 * Copyright (c) 2020
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
open Common
module CST = Tree_sitter_lua.CST
module H = Parse_tree_sitter_helpers
module PI = Parse_info
module G = AST_generic

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* LUA parser using tree-sitter-lang/semgrep-lua and converting
 * directly to pfff/h_program-lang/ast_generic.ml
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token

let str = H.str

let sc = PI.fake_info ";"

let fb = G.fake_bracket

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-lua/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the lua CST
   to another type of tree.
*)

let deoptionalize l =
  let rec deopt acc = function
    | [] -> List.rev acc
    | None :: tl -> deopt acc tl
    | Some x :: tl -> deopt (x :: acc) tl
  in
  deopt [] l

let mk_vars xs ys =
  let rec aux xs ys =
    match (xs, ys) with
    | [], [] -> []
    | x :: xs, [] ->
        (x, G.VarDef { G.vinit = None; G.vtype = None }) :: aux xs ys
    | x :: xs, y :: ys ->
        (x, G.VarDef { G.vinit = Some y; G.vtype = None }) :: aux xs ys
    | [], _y :: _ys -> []
  in
  aux xs ys

let mk_assigns xs ys equal =
  let rec aux xs ys =
    match (xs, ys) with
    | [], [] -> []
    | _x :: _xs, [] -> []
    | x :: xs, y :: ys -> G.Assign (x, equal, y) :: aux xs ys
    | [], _y :: _ys -> []
  in
  aux xs ys

let identifier (env : env) (tok : CST.identifier) : G.ident = str env tok

(* pattern [a-zA-Z_]\w* *)

let ident (env : env) (tok : CST.identifier) =
  G.Id (identifier env tok, G.empty_id_info ())

let string_literal (env : env) (tok : CST.identifier) =
  let s, t = str env tok in
  let s =
    (* tree-sitter-lua external 'string' rule keeps the enclosing quotes,
     * but AST_generic.String assumes the string does not contain the delimiter
     *)
    match s with
    | s when s =~ "^\"\\(.*\\)\"$" -> Common.matched1 s
    | _ ->
        logger#warning "weird string literal: %s" s;
        s
  in
  G.L (G.String (s, t))

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

let map_parameters (env : env) ((v1, v2, v3) : CST.parameters) : G.parameters =
  let _v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 =
          match v1 with
          | `Self tok ->
              G.ParamClassic (G.param_of_id (identifier env tok)) (* "self" *)
          | `Spread tok -> G.ParamEllipsis (token env tok) (* "..." *)
          | `Id tok -> G.ParamClassic (G.param_of_id (identifier env tok))
          (* pattern [a-zA-Z_][a-zA-Z0-9_]* *)
        in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = identifier env v2 (* pattern [a-zA-Z_][a-zA-Z0-9_]* *) in
              Some (G.ParamClassic (G.param_of_id v2)))
            v2
        in
        let v3 =
          match v3 with
          | Some (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = token env v2 (* "..." *) in
              Some (G.ParamEllipsis v2)
          | None -> None
        in
        deoptionalize (List.concat [ [ Some v1 ]; v2; [ v3 ] ])
    | None -> []
  in
  let _v3 = token env v3 (* ")" *) in
  v2

let map_local_variable_declarator (env : env)
    ((v1, v2) : CST.local_variable_declarator) local : G.entity list =
  let ident_first = identifier env v1 (* pattern [a-zA-Z_][a-zA-Z0-9_]* *) in
  let ident_rest =
    List.map
      (fun (v1, v2) ->
        let _comma = token env v1 (* "," *) in
        let ident = identifier env v2 (* pattern [a-zA-Z_][a-zA-Z0-9_]* *) in
        ident)
      v2
  in
  List.map
    (fun x -> G.basic_entity x [ G.KeywordAttr (G.Static, local) ])
    (ident_first :: ident_rest)

(* TODO: return a G.name *)
let map_function_name_field (env : env) ((v1, v2) : CST.function_name_field)
    colon_and_ident : G.ident * G.name_info =
  let v1 = identifier env v1 (* pattern [a-zA-Z_][a-zA-Z0-9_]* *) in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "." *) in
        let v2 = identifier env v2 (* pattern [a-zA-Z_][a-zA-Z0-9_]* *) in
        v2)
      v2
  in
  let list =
    match colon_and_ident with
    | Some (colon, colon_ident) ->
        let _colon = token env colon (* ":" *) in
        let colon_ident = identifier env colon_ident in
        List.flatten [ [ v1 ]; v2; [ colon_ident ] ]
    | None -> v1 :: v2
  in
  match List.rev list with
  | [] -> raise Impossible (* list1 *)
  | [ ident ] -> (ident, { G.name_qualifier = None; G.name_typeargs = None })
  | ident :: xs ->
      ( ident,
        {
          G.name_qualifier = Some (G.QDots (List.rev xs));
          G.name_typeargs = None;
        } )

(* TODO: return a G.name *)
let map_function_name (env : env) ((v1, v2) : CST.function_name) :
    G.ident * G.name_info =
  match v1 with
  | `Id tok ->
      let ident = identifier env tok in
      (* pattern [a-zA-Z_][a-zA-Z0-9_]* *)
      (ident, { G.name_qualifier = None; G.name_typeargs = None })
  | `Func_name_field x -> map_function_name_field env x v2

let rec map_expression_list (env : env)
    ((v1, v2) : CST.anon_exp_rep_COMMA_exp_0bb260c) : G.expr list =
  let v1 = map_expression env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = map_expression env v2 in
        v2)
      v2
  in
  v1 :: v2

and map_expression_tuple (env : env)
    ((v1, v2) : CST.anon_exp_rep_COMMA_exp_0bb260c) : G.expr =
  let v1 = map_expression_list env (v1, v2) in
  G.Tuple (G.fake_bracket v1)

and map_anon_arguments (env : env)
    ((v1, v2) : CST.anon_exp_rep_COMMA_exp_0bb260c) : G.arguments =
  let v1 = map_expression_list env (v1, v2) in
  List.map (fun (v1 : G.expr) -> G.Arg v1) v1

and map_arguments (env : env) (x : CST.arguments) : G.arguments G.bracket =
  match x with
  | `LPAR_opt_exp_rep_COMMA_exp_RPAR (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 =
        match v2 with Some x -> map_anon_arguments env x | None -> []
      in
      let v3 = token env v3 (* ")" *) in
      (v1, v2, v3)
  | `Table x -> fb [ G.Arg (map_table env x) ]
  | `Str tok -> fb [ G.Arg (string_literal env tok) ]

(* string *)
and map_binary_operation (env : env) (x : CST.binary_operation) =
  match x with
  | `Exp_or_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "or" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Or, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_and_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "and" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.And, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Lt, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.LtE, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Eq, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_TILDEEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "~=" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.NotEq, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.GtE, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Gt, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.BitOr, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_TILDE_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "~" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.BitNot, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.BitAnd, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<<" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.LSL, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">>" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.LSR, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Plus, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "-" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Minus, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Mult, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "/" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Div, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_SLASHSLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "//" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.FloorDiv, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "%" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Mod, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_DOTDOT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ".." *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Concat, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.BitXor, v2), fb [ G.Arg v1; G.Arg v3 ])

and map_statement_list (env : env) (x : CST.statement list) : G.stmt list =
  let v1 = List.map (map_statement env) x in
  List.flatten v1

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

and map_do_block (env : env) (v1, v2, v3, v4) : G.stmt =
  let v1 = token env v1 (* "do" *) in
  let v2 = map_statements_and_return env (v2, v3) in
  let v3 = token env v4 (* "end" *) in
  G.Block (v1, v2, v3) |> G.s

and map_else_ (env : env) ((v1, v2, v3) : CST.else_) : G.stmt =
  let _v1 = token env v1 (* "else" *) in
  let stmt_list = map_statements_and_return env (v2, v3) in
  G.Block (G.fake_bracket stmt_list) |> G.s

(* and map_elseif (env : env) ((v1, v2, v3, v4, v5) : CST.elseif) =
 *   let v1 = token env v1 (\* "elseif" *\) in
 *   let v2 = map_expression env v2 in
 *   let v3 = token env v3 (\* "then" *\) in
 *   let v4 = List.map (map_statement env) v4 in
 *   let v5 =
 *     (match v5 with
 *     | Some x -> map_return_statement env x
 *     | None -> todo env ())
 *   in
 *   todo env (v1, v2, v3, v4, v5) *)
and map_expression (env : env) (x : CST.expression) : G.expr =
  match x with
  | `Next tok -> G.OtherExpr (G.OE_Todo, [ G.TodoK ("next", token env tok) ])
  | `Spread tok -> G.Ellipsis (token env tok) (* "..." *)
  | `Prefix x -> map_prefix env x
  | `Func_defi (v1, v2) ->
      let _t = token env v1 (* "function" *) in
      let v2 = map_function_body env v2 v1 in
      G.Lambda v2
  | `Table x -> map_table env x
  | `Bin_oper x -> map_binary_operation env x
  | `Un_oper (v1, v2) ->
      let op, tok =
        match v1 with
        | `Not tok -> (G.Plus, tok) (* "not" *)
        | `HASH tok -> (G.Length, tok) (* "#" *)
        | `DASH tok -> (G.Minus, tok) (* "-" *)
        | `TILDE tok -> (G.BitNot, tok)
        (* "~" *)
      in
      let v2 = map_expression env v2 in
      G.Call (G.IdSpecial (G.Op op, token env tok), fb [ G.Arg v2 ])
  | `Str tok -> string_literal env tok (* string *)
  | `Num tok ->
      let s, tok = str env tok in
      G.L (G.Float (float_of_string_opt s, tok))
      (* number *)
  | `Nil tok -> G.L (G.Null (token env tok)) (* "nil" *)
  | `True tok -> G.L (G.Bool (true, token env tok)) (* "true" *)
  | `False tok -> G.L (G.Bool (false, token env tok)) (* "false" *)
  | `Id tok -> G.N (ident env tok)

(* pattern [a-zA-Z_][a-zA-Z0-9_]* *)
and map_field (env : env) (x : CST.field) : G.expr =
  let ent, tok, def =
    match x with
    | `LBRACK_exp_RBRACK_EQ_exp (v1, v2, v3, v4, v5) ->
        let _v1 = token env v1 (* "[" *) in
        let v2 = map_expression env v2 in
        let v3 = token env v3 (* "]" *) in
        let _v4 = token env v4 (* "=" *) in
        let v5 = map_expression env v5 in
        (v2, v3, v5)
    | `Id_EQ_exp (v1, v2, v3) ->
        let v1 = identifier env v1 (* pattern [a-zA-Z_][a-zA-Z0-9_]* *) in
        let v2 = token env v2 (* "=" *) in
        let v3 = map_expression env v3 in
        (G.N (G.Id (v1, G.empty_id_info ())), v2, v3)
    | `Exp x ->
        let expr = map_expression env x in
        let ident = G.IdSpecial (G.NextArrayIndex, G.fake "next_array_index") in
        (ident, G.fake "=", expr)
  in
  G.Assign (ent, tok, def)

and map_field_sequence (env : env) ((v1, v2, v3) : CST.field_sequence) :
    G.expr list =
  let v1 = map_field env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = map_field_sep env v1 in
        let v2 = map_field env v2 in
        v2)
      v2
  in
  let _v3 = match v3 with Some x -> [ map_field_sep env x ] | None -> [] in
  v1 :: v2

and map_function_body (env : env) ((v1, v2, v3, v4) : CST.function_body)
    (name : CST.identifier) : G.function_definition =
  let v1 = map_parameters env v1 in
  let body = map_do_block env (name, v2, v3, v4) in
  {
    G.fparams = v1;
    frettype = None;
    fkind = (G.Function, token env name);
    fbody = body;
  }

and map_function_call_expr (env : env) (x : CST.function_call_statement) :
    G.expr =
  match x with
  | `Prefix_args (v1, v2) ->
      let v1 = map_prefix env v1 in
      let v2 = map_arguments env v2 in
      G.Call (v1, v2)
  | `Prefix_COLON_id_args (v1, v2, v3, v4) ->
      let prefix = map_prefix env v1 in
      let colon = token env v2 (* ":" *) in
      let fn_name = identifier env v3 in
      (* pattern [a-zA-Z_][a-zA-Z0-9_]* *)
      let name =
        ( fn_name,
          {
            G.name_qualifier = Some (G.QExpr (prefix, colon));
            G.name_typeargs = None;
          } )
      in
      let args = map_arguments env v4 in
      G.Call (G.N (G.IdQualified (name, G.empty_id_info ())), args)

and map_function_call_statement (env : env) (x : CST.function_call_statement) :
    G.stmt =
  let expr = map_function_call_expr env x in
  G.ExprStmt (expr, sc) |> G.s

and map_in_loop_expression (env : env)
    ((v1, v2, v3, v4, v5) : CST.in_loop_expression) =
  let v1 = identifier env v1 (* pattern [a-zA-Z_][a-zA-Z0-9_]* *) in
  let var : G.variable_definition = { vinit = None; vtype = None } in
  let for_init_var = G.ForInitVar (G.basic_entity v1 [], var) in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = identifier env v2 (* pattern [a-zA-Z_][a-zA-Z0-9_]* *) in
        let var : G.variable_definition = { vinit = None; vtype = None } in
        G.ForInitVar (G.basic_entity v2 [], var))
      v2
  in
  (* TODO *)
  let _v3 = token env v3 (* "in" *) in
  let v4 = map_expression env v4 in
  let v5 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = map_expression env v2 in
        v2)
      v5
  in
  G.ForIn (for_init_var :: v2, v4 :: v5)

and map_loop_expression (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.loop_expression) =
  let v1 = identifier env v1 (* pattern [a-zA-Z_][a-zA-Z0-9_]* *) in
  let _v2 = token env v2 (* "=" *) in
  let _v3 = map_expression env v3 in
  let var : G.variable_definition = { vinit = None; vtype = None } in
  let for_init_var = G.ForInitVar (G.basic_entity v1 [], var) in
  let _v4 = token env v4 (* "," *) in
  let v5 = map_expression env v5 in
  let v6 =
    match v6 with
    | Some (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = map_expression env v2 in
        Some v2
    | None -> None
  in
  G.ForClassic ([ for_init_var ], Some v5, v6)

and map_global_variable (env : env) (x : CST.global_variable) : G.expr =
  match x with
  | `X__G t -> G.N (G.Id (("_G", token env t), G.empty_id_info ()))
  | `X__VERSION t -> G.N (G.Id (("_VERSION", token env t), G.empty_id_info ()))

and map_prefix (env : env) (x : CST.prefix) : G.expr =
  match x with
  | `Global_var x -> map_global_variable env x
  | `Self t -> G.IdSpecial (G.Self, token env t)
  | `Var_decl x -> map_variable_declarator_expr env x
  | `Func_call_stmt x -> map_function_call_expr env x
  | `LPAR_exp_RPAR (v1, v2, v3) ->
      let _v1 = token env v1 (* "(" *) in
      let v2 = map_expression env v2 in
      let _v3 = token env v3 (* ")" *) in
      v2

and map_return_statement (env : env) ((v1, v2, v3) : CST.return_statement) =
  let v1 = token env v1 (* "return" *) in
  let v2 =
    match v2 with Some x -> Some (map_expression_tuple env x) | None -> None
  in
  let v3 = match v3 with Some tok -> token env tok (* ";" *) | None -> sc in
  G.Return (v1, v2, v3) |> G.s

and map_statement (env : env) (x : CST.statement) : G.stmt list =
  match x with
  | `Exp x -> [ G.ExprStmt (map_expression env x, sc) |> G.s ]
  | `Var_decl (v1, v2, v3, v4, v5) ->
      let ident_first = map_variable_declarator env v1 in
      let ident_rest =
        List.map
          (fun (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            let v2 = map_variable_declarator env v2 in
            v2)
          v2
      in
      let equal = token env v3 (* "=" *) in
      let expr_first = map_expression env v4 in
      let expr_rest =
        List.map
          (fun (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            let v2 = map_expression env v2 in
            v2)
          v5
      in
      let assigns =
        mk_assigns (ident_first :: ident_rest) (expr_first :: expr_rest) equal
      in
      List.map (fun x -> G.ExprStmt (x, sc) |> G.s) assigns
  | `Local_var_decl (v1, v2, v3) ->
      let local = token env v1 (* "local" *) in
      let entities = map_local_variable_declarator env v2 local in
      let exprs =
        match v3 with
        | Some (v1, v2, v3) ->
            let _v1 = token env v1 (* "=" *) in
            let v2 = map_expression env v2 in
            let v3 =
              List.map
                (fun (v1, v2) ->
                  let _v1 = token env v1 (* "," *) in
                  let v2 = map_expression env v2 in
                  v2)
                v3
            in
            v2 :: v3
        | None -> []
      in
      let defs = mk_vars entities exprs in
      List.map (fun x -> G.DefStmt x |> G.s) defs
  | `Do_stmt (v1, v2, v3, v4) -> [ map_do_block env (v1, v2, v3, v4) ]
  | `If_stmt (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = token env v1 (* "if" *) in
      let v2 = map_expression env v2 in
      let _v3 = token env v3 (* "then" *) in
      let stmt_list =
        G.Block (G.fake_bracket (map_statements_and_return env (v4, v5))) |> G.s
      in
      let elseifs =
        List.fold_left
          (fun (acc : G.stmt option) ((v1, v2, v3, v4, v5) : CST.elseif) ->
            let v1 = token env v1 (* "elseif" *) in
            let v2 = map_expression env v2 in
            let _v3 = token env v3 (* "then" *) in
            let stmt_list =
              G.Block (G.fake_bracket (map_statements_and_return env (v4, v5)))
              |> G.s
            in
            Some (G.If (v1, v2, stmt_list, acc) |> G.s))
          None v6
      in
      let v7 =
        match v7 with Some x -> Some (map_else_ env x) | None -> None
      in
      let _v8 = token env v8 (* "end" *) in
      let ifstmt =
        match v7 with
        | Some else_ -> G.If (v1, v2, stmt_list, Some else_)
        | None -> G.If (v1, v2, stmt_list, elseifs)
      in
      [ ifstmt |> G.s ]
  | `While_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "while" *) in
      let v2 = map_expression env v2 in
      let block = map_do_block env (v3, v4, v5, v6) in
      [ G.While (v1, v2, block) |> G.s ]
  | `Repeat_stmt (v1, v2, v3, v4, v5) ->
      let t = token env v1 in
      (* "repeat" *)
      let block = map_do_block env (v1, v2, v3, v4) in
      let v5 = map_expression env v5 in
      [ G.DoWhile (t, block, v5) |> G.s ]
  | `For_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = map_loop_expression env v2 in
      let v3 = map_do_block env (v3, v4, v5, v6) in
      [ G.For (v1, v2, v3) |> G.s ]
  | `For_in_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = map_in_loop_expression env v2 in
      let block = map_do_block env (v3, v4, v5, v6) in
      [ G.For (v1, v2, block) |> G.s ]
  | `Goto_stmt (v1, v2) ->
      let v1 = token env v1 (* "goto" *) in
      let v2 = identifier env v2 (* pattern [a-zA-Z_][a-zA-Z0-9_]* *) in
      [ G.Goto (v1, v2) |> G.s ]
  | `Brk_stmt tok ->
      let v1 = token env tok (* "break" *) in
      [ G.Break (v1, LNone, sc) |> G.s ]
  | `Label_stmt (v1, v2, v3) ->
      let _v1 = token env v1 (* "::" *) in
      let v2 = identifier env v2 (* pattern [a-zA-Z_][a-zA-Z0-9_]* *) in
      let _v3 = token env v3 (* "::" *) in
      [ G.Label (v2, G.empty_fbody) |> G.s ]
  | `Empty_stmt _tok -> [] (* ";" *)
  | `Func_stmt (v1, v2, v3) ->
      let fn_name = map_function_name env v2 in
      let name = G.IdQualified (fn_name, G.empty_id_info ()) in
      let v3 = map_function_body env v3 v1 in
      let ent = { G.name = G.EN name; G.attrs = []; G.tparams = [] } in
      [ G.DefStmt (ent, G.FuncDef v3) |> G.s ]
  | `Local_func_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "local" *) in
      let _tok = token env v2 (* "function" *) in
      let v3 = identifier env v3 (* pattern [a-zA-Z_][a-zA-Z0-9_]* *) in
      let v4 = map_function_body env v4 v2 in
      let ent = G.basic_entity v3 [ G.KeywordAttr (G.Static, v1) ] in
      [ G.DefStmt (ent, G.FuncDef v4) |> G.s ]
  | `Func_call_stmt x -> [ map_function_call_statement env x ]

and map_table (env : env) ((v1, v2, v3) : CST.table) : G.expr =
  let v1 = token env v1 (* "{" *) in
  let v2 = match v2 with Some x -> map_field_sequence env x | None -> [] in
  let v3 = token env v3 (* "}" *) in
  G.Container (G.Dict, (v1, v2, v3))

and map_variable_declarator_expr (env : env) (x : CST.variable_declarator) :
    G.expr =
  match x with
  | `Id tok -> G.N (ident env tok) (* pattern [a-zA-Z_][a-zA-Z0-9_]* *)
  | `Prefix_LBRACK_exp_RBRACK (v1, v2, v3, v4) ->
      let v1 = map_prefix env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 = map_expression env v3 in
      let v4 = token env v4 (* "]" *) in
      let _qual = G.QExpr (v1, v4) in
      let expr = G.ArrayAccess (v1, (v2, v3, v4)) in
      expr
  | `Field_exp (v1, v2, v3) ->
      let v1 = map_prefix env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = identifier env v3 (* pattern [a-zA-Z_][a-zA-Z0-9_]* *) in
      let qual = G.QExpr (v1, v2) in
      let name =
        (v3, { G.name_qualifier = Some qual; G.name_typeargs = None })
      in
      G.N (G.IdQualified (name, G.empty_id_info ()))

and map_variable_declarator (env : env) (x : CST.variable_declarator) : G.expr =
  match x with
  | `Id tok ->
      G.N (G.Id (identifier env tok, G.empty_id_info ()))
      (* pattern [a-zA-Z_][a-zA-Z0-9_]* *)
  | `Prefix_LBRACK_exp_RBRACK (v1, v2, v3, v4) ->
      let v1 = map_prefix env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 = map_expression env v3 in
      let v4 = token env v4 (* "]" *) in
      let _qual = G.QExpr (v1, v4) in
      G.ArrayAccess (v1, (v2, v3, v4))
  | `Field_exp (v1, v2, v3) ->
      let prefix = map_prefix env v1 in
      let dot = token env v2 (* "." *) in
      let ident = identifier env v3 (* pattern [a-zA-Z_][a-zA-Z0-9_]* *) in
      G.DotAccess
        (G.N (G.Id (ident, G.empty_id_info ())), dot, G.EDynamic prefix)

let map_program (env : env) ((v1, v2) : CST.program) : G.program =
  map_statements_and_return env (v1, v2)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () ->
      Parallel.backtrace_when_exn := false;
      Parallel.invoke Tree_sitter_lua.Parse.file file ())
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in

      try map_program env cst
      with Failure "not implemented" as exn ->
        let s = Printexc.get_backtrace () in
        pr2 "Some constructs are not handled yet";
        pr2 "CST was:";
        CST.dump_tree cst;
        pr2 "Original backtrace:";
        pr2 s;
        raise exn)

(* todo: special mode to convert Ellipsis in the right construct! *)
let parse_pattern str =
  H.wrap_parser
    (fun () ->
      Parallel.backtrace_when_exn := false;
      Parallel.invoke Tree_sitter_lua.Parse.string str ())
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = () } in
      match map_program env cst with
      | [ { s = G.ExprStmt (e, _); _ } ] -> G.E e
      | [ x ] -> G.S x
      | xs -> G.Ss xs)
