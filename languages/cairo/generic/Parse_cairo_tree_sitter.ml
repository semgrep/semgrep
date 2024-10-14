(* Romain J
 *
 * Copyright (c) 2023 Semgrep Inc.
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
module Token = Tree_sitter_run.Token
module CST = Tree_sitter_cairo.CST
module H = Parse_tree_sitter_helpers
module H2 = AST_generic_helpers
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Cairo parser using tree-sitter-lang/semgrep-cairo and converting directly
 * to AST_generic.ml
 *
 * Cairo is a language based on Rust, therefore this transformer has been
 * inspired by the Rust transformer The code below "entry points" has been
 * copy-pasted from the Rust transformer but everything else has been rewritten
 * to accomodate the syntax and semantic of the language which differ from
 * Rust's.
 *)

type mode = Pattern | Target
type env = mode H.env

let token = H.token
let str = H.str

(************* Helpers *************)

(* map a list of zero or more tokens that are separated by another token (ex. comma) *)
let map_list (f : 'a -> 'b) (x : ('a * Token.t) list) (tail : 'a option) :
    'b list =
  let rec remove_separator (x : ('a * Token.t) list) =
    match x with
    | (head, _) :: tail -> head :: remove_separator tail
    | [] -> []
  in

  let values =
    match tail with
    | Some tail -> remove_separator x @ [ tail ]
    | None -> remove_separator x
  in

  List_.map f values

(* map a list of one or more tokens that are separated by another token (ex. comma) *)
let map_list_1 (f : 'a -> 'b) (head : 'a) (x : (Token.t * 'a) list) : 'b list =
  let rec remove_separator (x : (Token.t * 'a) list) =
    match x with
    | (_, head) :: tail -> head :: remove_separator tail
    | [] -> []
  in

  List_.map f (head :: remove_separator x)

(* Convenience function to create a bracketed type. *)
let wrap_in (env : env) (x : Token.t * 'a * Token.t) : 'a G.bracket =
  let l, inner, r = x in
  (token env l, inner, token env r)

(************* Names *************)
let map_name (env : env) (x : CST.name) : G.ident =
  match x with
  | `Pat_7fdeb71 x -> str env x
  | `Semg_var x -> str env x

let name_to_token (env : env) (x : CST.name) =
  match x with
  | `Pat_7fdeb71 x -> token env x
  | `Semg_var x -> token env x

let map_name_to_entity_name (env : env) (x : CST.name) : G.entity_name =
  G.EN (H2.name_of_id (map_name env x))

let map_path (env : env) (x : CST.path) : G.dotted_ident =
  let v1, v2, _ = x in
  map_list_1 (map_name env) v1 v2

let map_path_to_ident (env : env) (x : CST.path) : G.ident =
  let v1, v2, _ = x in
  let path_segments = map_list_1 (map_name env) v1 v2 in
  let name =
    List.fold_left
      (fun a (s, _) -> String.concat "::" [ a; s ])
      "" path_segments
  in

  (name, name_to_token env v1)

let rec map_qualified_name (env : env) (x : CST.qualified_name) :
    G.qualified_info =
  let map_segment (x : CST.qualified_name_segment) :
      G.ident * G.type_arguments option =
    let name, type_arguments = x in
    let name = map_name env name in

    match type_arguments with
    | Some arguments -> (name, Some (map_type_arguments env arguments))
    | None -> (name, None)
  in

  let path, last_name, type_arguments = x in
  let path = map_list map_segment path None in
  let last_name = map_name env last_name in
  let type_arguments =
    match type_arguments with
    | Some (_, arguments) -> Some (map_type_arguments env arguments)
    | None -> None
  in

  {
    name_last = (last_name, type_arguments);
    name_middle = Some (G.QDots path);
    name_top = None;
    name_info = G.empty_id_info ();
  }

and map_type_arguments (env : env) (x : CST.type_argument_list) :
    G.type_arguments =
  let map_type_argument (x : CST.literal_expression) : G.type_argument =
    G.TAExpr (G.e (G.L (map_literal env x)))
  in

  let lb, v1, v2, _, rb = x in
  (token env lb, map_list_1 map_type_argument v1 v2, token env rb)

and map_modifiers (env : env) (x : CST.modifier list) =
  match x with
  | `Modi_mut tok :: tail ->
      G.KeywordAttr (G.Mutable, token env tok) :: map_modifiers env tail
  | _ :: tail -> map_modifiers env tail
  | [] -> []

and map_pattern (env : env) (x : CST.pattern) : G.pattern =
  let map_pattern_var (x : CST.pattern_var) : G.pattern =
    match x with
    | `Wild x -> G.PatWildcard (token env x)
    | `Choice_pat_7fdeb71 x -> G.PatId (map_name env x, G.empty_id_info ())
  in

  let map_pattern_tuple (x : CST.pattern_tuple) : G.pattern =
    let lb, v1, v2, _, rb = x in
    let values = map_list_1 (map_pattern env) v1 v2 in

    G.PatTuple (token env lb, values, token env rb)
  in

  let map_pattern_enum (x : CST.pattern_enum) =
    let name, _, v1, v2, _, _ = x in
    let name = map_qualified_name env name in
    let values = map_list_1 (map_pattern env) v1 v2 in

    G.PatConstructor (G.IdQualified name, values)
  in

  let map_pattern_struct (x : CST.pattern_struct) =
    let map_field_pattern (x : CST.pattern_struct_binding) :
        G.dotted_ident * G.pattern =
      match x with
      | `Choice_pat_7fdeb71_COLON_choice_choice_wild (name, _, pattern) ->
          ([ map_name env name ], map_pattern env pattern)
      | `Choice_pat_7fdeb71 name ->
          ( [ map_name env name ],
            G.PatId (map_name env name, G.empty_id_info ()) )
    in

    let _, lb, v1, v2, _, rb = x in
    let fields = map_list_1 map_field_pattern v1 v2 in

    G.PatRecord (token env lb, fields, token env rb)
  in

  match x with
  | `Choice_wild x -> map_pattern_var x
  | `Pat_tuple x -> map_pattern_tuple x
  | `Pat_struct x -> map_pattern_struct x
  | `Pat_enum x -> map_pattern_enum x

and map_type (env : env) (x : CST.type_) : G.type_ =
  let map_unit_type (x : CST.unit_type) : G.type_ =
    G.TyN (G.Id (("unit_type", token env x), G.empty_id_info ())) |> G.t
  in

  let map_type_id (x : CST.type_identifier) : G.type_ =
    let map_type_arg (x : CST.type_) = G.TA (map_type env x) in

    let name, args = x in
    let name = G.TyN (G.IdQualified (map_qualified_name env name)) |> G.t in
    match args with
    | Some (lb, v1, v2, rb) ->
        let args = (token env lb, map_list map_type_arg v1 v2, token env rb) in
        G.TyApply (name, args) |> G.t
    | None -> name
  in

  let map_type_tuple (x : CST.type_tuple) =
    let lb, v1, v2, rb = x in
    let types = map_list (map_type env) v1 v2 in

    G.TyTuple (token env lb, types, token env rb) |> G.t
  in

  match x with
  | `Unit_type x -> map_unit_type x
  | `Type_id x -> map_type_id x
  | `Type_tuple x -> map_type_tuple x

and map_deep_ellipsis (env : env) (x : CST.deep_ellipsis) =
  let l, expr, r = x in
  let lellips = token env l in
  let expr = map_expression env expr in
  let rellips = token env r in

  G.DeepEllipsis (lellips, expr, rellips)

and map_expression (env : env) (x : CST.expression) =
  match x with
  | `Tuple_exp x -> map_tuple_expression env x
  | `Blk x -> map_block env x
  | `Un_exp x -> map_unary_expression env x
  | `Bin_exp x -> map_binary_expression env x
  | `If_exp x -> map_if_expression env x
  | `Loop_exp x -> map_loop_expression env x
  | `Match_exp x -> map_match_expression env x
  | `Sele_exp x -> map_selector_expression env x
  | `Call_exp x -> map_call_expression env x
  | `Qual_name x -> G.N (G.IdQualified (map_qualified_name env x)) |> G.e
  | `Choice_true x -> G.L (map_literal env x) |> G.e
  | `Ellips x -> G.Ellipsis (token env x) |> G.e
  | `Deep_ellips x -> map_deep_ellipsis env x |> G.e

and map_simple_expression (env : env) (x : CST.simple_expression) : G.expr =
  match x with
  | `Tuple_exp x -> map_tuple_expression env x
  | `Un_exp x -> map_unary_expression env x
  | `Bin_exp x -> map_binary_expression env x
  | `If_exp x -> map_if_expression env x
  | `Loop_exp x -> map_loop_expression env x
  | `Match_exp x -> map_match_expression env x
  | `Sele_exp x -> map_selector_expression env x
  | `Call_exp x -> map_call_expression env x
  | `Qual_name x -> G.N (G.IdQualified (map_qualified_name env x)) |> G.e
  | `Choice_true x -> G.L (map_literal env x) |> G.e
  | `Ellips x -> G.Ellipsis (token env x) |> G.e
  | `Deep_ellips x -> map_deep_ellipsis env x |> G.e

and map_statement (env : env) (x : CST.statement) : G.stmt =
  match x with
  | `Let_stmt x -> map_let_statement env x
  | `Assign_stmt x -> map_assignment_statement env x
  | `Ret_stmt x -> map_return_statement env x
  | `Brk_stmt x -> map_break_statement env x
  | `If_exp x -> map_if_expression env x |> G.exprstmt
  | `Loop_exp x -> map_loop_expression env x |> G.exprstmt
  | `Match_exp x -> map_match_expression env x |> G.exprstmt
  | `Choice_tuple_exp_SEMI (x, _) -> map_expression env x |> G.exprstmt
  | `Ellips x -> G.Ellipsis (token env x) |> G.e |> G.exprstmt
  | `Deep_ellips x -> map_deep_ellipsis env x |> G.e |> G.exprstmt

and map_tuple_expression (env : env) (x : CST.tuple_expression) =
  let lp, v1, v2, _, rp = x in
  let lparen = token env lp in
  let rparen = token env rp in

  let exprs = map_list_1 (map_expression env) v1 v2 in
  G.e (G.Container (G.Tuple, (lparen, exprs, rparen)))

and map_let_statement (env : env) (x : CST.let_statement) : G.stmt =
  let _, _, pattern, _, _, expr, _ = x in
  let pattern = map_pattern env pattern and expr = map_expression env expr in

  G.e (G.LetPattern (pattern, expr)) |> G.exprstmt

and map_loop_expression (env : env) (x : CST.loop_expression) : G.expr =
  let loop, block = x in

  (* loop don't have condition in cairo so we mimic this with a condition = true *)
  let ttrue = G.e (G.L (G.Bool (true, token env loop))) in
  let body = map_block env block in
  G.stmt_to_expr (G.s (G.While (token env loop, G.Cond ttrue, G.exprstmt body)))

and map_selector_expression (env : env) (x : CST.selector_expression) =
  match x with
  | `Choice_tuple_exp_DOT_choice_pat_7fdeb71 (expr, dot, field) ->
      let expr = map_expression env expr in
      let field = H2.name_of_id (map_name env field) in

      G.DotAccess (expr, token env dot, G.FN field) |> G.e
  | `Choice_tuple_exp_DOT_ellips (expr, dot, _) ->
      let expr = map_expression env expr in
      G.DotAccessEllipsis (expr, token env dot) |> G.e

and map_call_expression (env : env) (x : CST.call_expression) =
  let map_argument (x : CST.expression) = G.Arg (map_expression env x) in

  let expr, (lp, v1, v2, rp) = x in
  let expr = map_expression env expr in
  let arguments = map_list map_argument v1 v2 in

  G.Call (expr, (token env lp, arguments, token env rp)) |> G.e

and map_if_expression (env : env) (x : CST.if_expression) : G.expr =
  let iff, cond, thenn, elze = x in

  let cond = map_simple_expression env cond
  and thenn = G.ExprStmt (map_block env thenn, G.sc) |> G.s
  and elze =
    match elze with
    | Some (_, `If_exp e) ->
        Some (G.ExprStmt (map_if_expression env e, G.sc) |> G.s)
    | Some (_, `Blk e) -> Some (G.ExprStmt (map_block env e, G.sc) |> G.s)
    | None -> None
  in

  let if_stmt = G.If (token env iff, Cond cond, thenn, elze) |> G.s in
  G.stmt_to_expr if_stmt

and map_match_expression (env : env) (x : CST.match_expression) =
  let map_case (x : CST.match_case) =
    let pattern, arrow, expr = x in
    let body = map_expression env expr in

    let case =
      match pattern with
      | `Choice_choice_wild x -> G.Case (token env arrow, map_pattern env x)
      | `Choice_true x ->
          G.CaseEqualExpr (token env arrow, G.L (map_literal env x) |> G.e)
    in

    G.CasesAndBody ([ case ], G.exprstmt body)
  in

  let mmatch, condition, (_, v1, v2, _) = x in
  let condition = map_expression env condition in
  let cases = map_list map_case v1 v2 in

  G.stmt_to_expr
    (G.Switch (token env mmatch, Some (G.Cond condition), cases) |> G.s)

and map_block (env : env) (x : CST.block) : G.expr =
  let lb, prevs, last, rb = x in

  let lb = token env lb
  and rb = token env rb
  and statements = List_.map (map_statement env) prevs
  and final =
    match last with
    | Some expr -> [ G.ExprStmt (map_expression env expr, G.sc) |> G.s ]
    | None -> []
  in

  G.stmt_to_expr (G.Block (lb, statements @ final, rb) |> G.s)

and map_assignment_statement (env : env) (x : CST.assignment_statement) : G.stmt
    =
  let lhs, operator, rhs, _ = x in
  let lhs =
    match lhs with
    | `Wild x -> G.N (G.Id (("_", token env x), G.empty_id_info ())) |> G.e
    | `Choice_pat_7fdeb71 x -> G.N (H2.name_of_id (map_name env x)) |> G.e
  in
  let rhs = map_expression env rhs in

  let expr =
    match operator with
    | `DASHEQ x -> G.AssignOp (lhs, (G.Minus, token env x), rhs)
    | `PLUSEQ x -> G.AssignOp (lhs, (G.Plus, token env x), rhs)
    | `SLASHEQ x -> G.AssignOp (lhs, (G.Div, token env x), rhs)
    | `STAREQ x -> G.AssignOp (lhs, (G.Mult, token env x), rhs)
    | `PERCEQ x -> G.AssignOp (lhs, (G.Mod, token env x), rhs)
    | `EQ x -> G.Assign (lhs, token env x, rhs)
  in

  expr |> G.e |> G.exprstmt

and map_return_statement (env : env) (x : CST.return_statement) : G.stmt =
  let return, expr, sc = x in

  let stmt =
    match expr with
    | Some expr ->
        G.Return (token env return, Some (map_expression env expr), token env sc)
    | None -> G.Return (token env return, None, token env sc)
  in

  stmt |> G.s

and map_break_statement (env : env) (x : CST.break_statement) =
  let break, expr, sc = x in

  let stmt =
    match expr with
    | Some expr ->
        G.Break
          (token env break, G.LDynamic (map_expression env expr), token env sc)
    | None -> G.Break (token env break, G.LNone, token env sc)
  in

  stmt |> G.s

and map_unary_expression (env : env) (x : CST.unary_expression) : G.expr =
  let op, expr = x in
  let expr = map_expression env expr in
  match op with
  | `BANG tok -> G.opcall (G.Not, token env tok) [ expr ]
  | `STAR tok -> G.e (G.DeRef (token env tok, expr))
  | `DASH tok -> G.opcall (G.Minus, token env tok) [ expr ]

and map_binary_expression (env : env) (x : CST.binary_expression) =
  match x with
  | `Choice_tuple_exp_choice_STAR_choice_tuple_exp (lhs, op, rhs) ->
      let lhs = map_expression env lhs
      and rhs = map_expression env rhs
      and op =
        match op with
        | `STAR tok -> (G.Mult, token env tok)
        | `SLASH tok -> (G.Div, token env tok)
        | `PERC tok -> (G.Mod, token env tok)
      in

      G.opcall op [ lhs; rhs ]
  | `Choice_tuple_exp_choice_PLUS_choice_tuple_exp (lhs, op, rhs) ->
      let lhs = map_expression env lhs
      and rhs = map_expression env rhs
      and op =
        match op with
        | `PLUS tok -> (G.Plus, token env tok)
        | `DASH tok -> (G.Minus, token env tok)
      in

      G.opcall op [ lhs; rhs ]
  | `Choice_tuple_exp_choice_EQEQ_choice_tuple_exp (lhs, op, rhs) ->
      let lhs = map_expression env lhs
      and rhs = map_expression env rhs
      and op =
        match op with
        | `EQEQ tok -> (G.Eq, token env tok)
        | `BANGEQ tok -> (G.NotEq, token env tok)
        | `LT tok -> (G.Lt, token env tok)
        | `LTEQ tok -> (G.LtE, token env tok)
        | `GT tok -> (G.Gt, token env tok)
        | `GTEQ tok -> (G.GtE, token env tok)
      in

      G.opcall op [ lhs; rhs ]
  | `Choice_tuple_exp_AMPAMP_choice_tuple_exp (lhs, tok, rhs) ->
      let lhs = map_expression env lhs and rhs = map_expression env rhs in

      G.opcall (G.And, token env tok) [ lhs; rhs ]
  | `Choice_tuple_exp_BARBAR_choice_tuple_exp (lhs, tok, rhs) ->
      let lhs = map_expression env lhs and rhs = map_expression env rhs in

      G.opcall (G.Or, token env tok) [ lhs; rhs ]

and integer_literal env tok =
  let s, t = str env tok in
  Parsed_int.parse (s, t)

and map_literal (env : env) (x : CST.literal_expression) : G.literal =
  match x with
  | `True tok -> G.Bool (true, token env tok)
  | `False tok -> G.Bool (false, token env tok)
  | `Str x ->
      let _, s = x in
      G.String (G.fake "'", (s, token env x), G.fake "'")
  | `Num (`Int value, _) -> G.Int (integer_literal env value)
  | `Num (`Hex value, _) -> G.Int (integer_literal env value)
  | `Num (`Bin value, _) -> G.Int (integer_literal env value)
  | `Num (`Octal value, _) -> G.Int (integer_literal env value)
  (* TODO: simplify in grammar as single token *)
  | `Unit x -> G.Unit (token env x)

type function_signature = (G.entity * G.parameters * G.type_ option) G.wrap

let rec map_declaration (env : env) (x : CST.declaration) : G.stmt =
  let definition =
    match x with
    | `Import_decl x -> G.DirectiveStmt (map_import_declaration env x)
    | `Module_decl x -> G.DefStmt (map_module_declaration env x)
    | `Func_decl x -> DefStmt (map_function_declaration env x)
    | `Const_decl x -> DefStmt (map_const_declaration env x)
    | `Typeas_decl x -> DefStmt (map_typealias_declaration env x)
    | `Trait_decl x -> DefStmt (map_trait_declaration env x)
    | `Struct_decl x -> DefStmt (map_struct_declaration env x)
    | `Enum_decl x -> DefStmt (map_enum_declaration env x)
    | `Choice_impl_base x -> DefStmt (map_impl_declaration env x)
    | `Ellips x -> G.ExprStmt (G.e (G.Ellipsis (token env x)), G.sc)
  in

  definition |> G.s

and map_attributes (env : env) (x : CST.attribute_list list) : G.attribute list
    =
  let rec map_argument (x : CST.attribute_argument) : G.argument =
    match x with
    | `Choice_true x -> G.Arg (G.e (G.L (map_literal env x)))
    | `Choice_pat_7fdeb71 x ->
        G.Arg (G.e (G.N (H2.name_of_id (map_name env x))))
    | `Path_COLON_choice_true (path, _, x) ->
        G.ArgKwd (map_path_to_ident env path, G.e (G.L (map_literal env x)))
    | `Path_COLON_attr_arg_list (path, _, (lp, v1, v2, _, rb)) ->
        let arguments =
          map_list_1 (fun x -> H2.argument_to_expr (map_argument x)) v1 v2
        in
        let arguments =
          G.Container (G.Tuple, (token env lp, arguments, token env rb)) |> G.e
        in

        G.ArgKwd (map_path_to_ident env path, arguments)
    | `Ellips x -> G.Arg (G.Ellipsis (token env x) |> G.e)
  in

  let map_attribute (x : CST.attribute) : G.attribute =
    let path, arguments = x in

    let path = H2.name_of_id (map_path_to_ident env path) in

    let arguments =
      match arguments with
      | Some (lp, v1, v2, _, rp) ->
          (token env lp, map_list_1 map_argument v1 v2, token env rp)
      | None -> (G.fake "(", [], G.fake ")")
    in

    G.NamedAttr (G.fake "", path, arguments)
  in

  let map_attribute_list (x : CST.attribute_list) =
    let _, _, attributes, _ = x in
    List_.map map_attribute attributes
  in

  List_.flatten (List_.map map_attribute_list x)

and map_type_parameters (env : env) (x : CST.type_parameter_list) :
    G.type_parameters =
  let map_type_name (x : CST.name) : G.type_parameter =
    G.TP
      {
        tp_id = map_name env x;
        tp_bounds = [];
        tp_default = None;
        tp_attrs = [];
        tp_variance = None;
      }
  in

  let map_const_type (x : CST.type_const_declaration) : G.type_parameter =
    let const, name, _, ttype = x in
    G.TP
      {
        tp_id = map_name env name;
        tp_bounds = [ map_type env ttype ];
        tp_default = None;
        tp_attrs = [ G.KeywordAttr (G.Const, token env const) ];
        tp_variance = None;
      }
  in

  let map_impl_type (x : CST.type_impl_declaration) : G.type_parameter =
    let _, name, _, ttype = x in
    G.TP
      {
        tp_id = map_name env name;
        tp_bounds = [ map_type env ttype ];
        tp_default = None;
        tp_attrs = [];
        tp_variance = None;
      }
  in

  let map_type_parameter (x : CST.type_parameter_declaration) : G.type_parameter
      =
    match x with
    | `Choice_pat_7fdeb71 x -> map_type_name x
    | `Type_const_decl x -> map_const_type x
    | `Type_impl_decl x -> map_impl_type x
  in

  let lt, v1, v2, _sc, gt = x in
  let lt = token env lt in
  let gt = token env gt in
  (lt, map_list_1 map_type_parameter v1 v2, gt)

and map_function_signature (env : env) (x : CST.function_signature) :
    function_signature =
  let map_param (x : CST.parameter_declaration) : G.parameter =
    match x with
    | `Ellips x -> G.ParamEllipsis (token env x)
    | `Rep_choice_modi_ref_choice_pat_7fdeb71_COLON_choice_type_tuple
        (modifiers, param_name, _, ttype) ->
        let name = map_name env param_name in
        let ttype = map_type env ttype in
        let attributes = map_modifiers env modifiers in

        G.Param
          {
            pname = Some name;
            ptype = Some ttype;
            pdefault = None;
            pattrs = attributes;
            pinfo = G.empty_id_info ();
          }
  in

  let attributes, fn, fn_name, type_parameters, parameters, return_type = x in
  let fn_name : G.entity =
    {
      name = map_name_to_entity_name env fn_name;
      attrs = map_attributes env attributes;
      tparams =
        (match type_parameters with
        | Some parameters -> Some (map_type_parameters env parameters)
        | None -> None);
    }
  in

  let lb, v1, v2, rb = parameters in
  let parameters = (token env lb, map_list map_param v1 v2, token env rb) in

  let return_type =
    match return_type with
    | Some (_, ttype) -> Some (map_type env ttype)
    | None -> None
  in

  ((fn_name, parameters, return_type), token env fn)

and map_function_declaration (env : env) (x : CST.function_declaration) :
    G.definition =
  let signature, body = x in

  let (entity, params, return_type), tok =
    map_function_signature env signature
  in

  let body = map_block env body in

  ( entity,
    G.FuncDef
      {
        fkind = (G.Function, tok);
        fparams = params;
        frettype = return_type;
        fbody = G.FBStmt (G.exprstmt body);
      } )

and map_import_declaration (env : env) (x : CST.import_declaration) :
    G.directive =
  let use, path, alias, _ = x in
  let path = G.DottedName (map_path env path) in

  let directive =
    match alias with
    | Some (_, alias) ->
        G.ImportAs
          (token env use, path, Some (map_name env alias, G.empty_id_info ()))
    | None -> G.ImportAs (token env use, path, None)
  in

  { d = directive; d_attrs = [] }

and map_module_declaration (env : env) (x : CST.module_declaration) :
    G.definition =
  let attributes, _, name, body = x in

  let entity : G.entity =
    {
      name = map_name_to_entity_name env name;
      tparams = None;
      attrs = map_attributes env attributes;
    }
  in

  let body =
    match body with
    | `Module_body (_, body, _) -> List_.map (map_declaration env) body
    | `SEMI _ -> []
  in

  ( entity,
    G.ModuleDef { mbody = G.ModuleStruct (Some [ map_name env name ], body) } )

and map_const_declaration (env : env) (x : CST.const_declaration) : G.definition
    =
  let attributes, tconst, name, _, ttype, _, value, sc = x in
  let tconst = token env tconst in
  let name : G.entity =
    {
      name = G.EN (H2.name_of_id (map_name env name));
      attrs = map_attributes env attributes @ [ KeywordAttr (Const, tconst) ];
      tparams = None;
    }
  in
  let value = map_expression env value in
  let ttype = map_type env ttype in
  let sc = token env sc in
  (name, G.VarDef { vinit = Some value; vtype = Some ttype; vtok = Some sc })

and map_typealias_declaration (env : env) (x : CST.typealias_declaration) :
    G.definition =
  let _, name, tparams, _, ttype, sc = x in
  let name : G.entity =
    {
      name = map_name_to_entity_name env name;
      attrs = [];
      tparams = Option.map (map_type_parameters env) tparams;
    }
  in
  let ttype = map_type env ttype in
  let _sc = token env sc in
  (name, G.TypeDef { tbody = G.AliasType ttype })

and map_trait_declaration (env : env) (x : CST.trait_declaration) : G.definition
    =
  let map_function (x : CST.trait_function) : G.field =
    let signature, body = x in

    let (entity, parameters, return_type), tok =
      map_function_signature env signature
    in

    let body =
      match body with
      | `Blk block -> Some (map_block env block |> G.exprstmt)
      | _ -> None
    in

    let definition =
      G.s
        (G.DefStmt
           ( entity,
             G.FuncDef
               {
                 fkind = (G.Function, tok);
                 fparams = parameters;
                 frettype = return_type;
                 fbody =
                   (match body with
                   | Some body -> G.FBStmt body
                   | None -> G.FBNothing);
               } ))
    in

    G.F definition
  in

  let attributes, trait, name, tparams, (lb, functions, rb) = x in
  let entity : G.entity =
    {
      name = map_name_to_entity_name env name;
      attrs = map_attributes env attributes;
      tparams = Option.map (map_type_parameters env) tparams;
    }
  in

  let functions = List_.map map_function functions in

  ( entity,
    G.ClassDef
      {
        ckind = (G.Trait, token env trait);
        cextends = [];
        cimplements = [];
        cmixins = [];
        cparams = (G.fake "", [], G.fake "");
        cbody = wrap_in env (lb, functions, rb);
      } )

and map_struct_declaration (env : env) (x : CST.struct_declaration) :
    G.definition =
  let map_fields (env : env) (x : CST.member_declaration_list) :
      G.field list G.bracket =
    let map_field (x : CST.member_declaration) : G.field =
      match x with
      | `Rep_attr_list_choice_pat_7fdeb71_COLON_choice_type_tuple
          (attributes, name, _, ttype) ->
          let entity : G.entity =
            {
              name = map_name_to_entity_name env name;
              attrs = map_attributes env attributes;
              tparams = None;
            }
          in

          let definition =
            ( entity,
              G.FieldDefColon
                {
                  vinit = None;
                  vtype = Some (map_type env ttype);
                  vtok = G.no_sc;
                } )
          in

          G.F (G.s (G.DefStmt definition))
      | `Ellips x -> G.field_ellipsis (token env x)
    in

    let lb, v1, v2, rb = x in
    wrap_in env (lb, map_list map_field v1 v2, rb)
  in

  let attributes, sstruct, name, tparams, fields = x in

  let entity : G.entity =
    {
      name = map_name_to_entity_name env name;
      attrs = map_attributes env attributes;
      tparams = Option.map (map_type_parameters env) tparams;
    }
  in

  let fields =
    match fields with
    | `Member_decl_list fields -> map_fields env fields
    | `SEMI _ -> (G.fake "{", [], G.fake "}")
  in

  ( entity,
    G.ClassDef
      {
        ckind = (G.Class, token env sstruct);
        cextends = [];
        cimplements = [];
        cmixins = [];
        cparams = (G.fake "", [], G.fake "");
        cbody = fields;
      } )

and map_enum_declaration (env : env) (x : CST.enum_declaration) =
  let map_variants (env : env) (x : CST.member_declaration_list) :
      G.field list G.bracket =
    let map_variant (x : CST.member_declaration) : G.field =
      match x with
      | `Rep_attr_list_choice_pat_7fdeb71_COLON_choice_type_tuple
          (attributes, name, _, ttype) ->
          let entity : G.entity =
            {
              name = map_name_to_entity_name env name;
              attrs = map_attributes env attributes;
              tparams = None;
            }
          in

          let variant =
            ( entity,
              G.EnumEntryDef
                {
                  ee_args =
                    Some
                      ( G.fake "(",
                        [ G.ArgType (map_type env ttype) ],
                        G.fake ")" );
                  ee_body = None;
                } )
          in

          G.F (G.s (G.DefStmt variant))
      | `Ellips _ -> failwith "should not happen ?"
    in

    let lb, v1, v2, rb = x in
    wrap_in env (lb, map_list map_variant v1 v2, rb)
  in

  let attributes, enum, name, tparams, variants = x in
  let entity : G.entity =
    {
      name = map_name_to_entity_name env name;
      attrs =
        G.KeywordAttr (G.EnumClass, token env enum)
        :: map_attributes env attributes;
      tparams = Option.map (map_type_parameters env) tparams;
    }
  in

  let variants = map_variants env variants in

  ( entity,
    G.ClassDef
      {
        ckind = (G.Class, token env enum);
        cextends = [];
        cimplements = [];
        cmixins = [];
        cparams = (G.fake "", [], G.fake "");
        cbody = variants;
      } )

and map_impl_declaration (env : env) (x : CST.impl_declaration) : G.definition =
  let map_impl_trait (x : CST.impl_trait) : G.definition =
    let attributes, impl, name, tparams, _, trait, body = x in

    let entity : G.entity =
      {
        name = map_name_to_entity_name env name;
        attrs = map_attributes env attributes;
        tparams = Option.map (map_type_parameters env) tparams;
      }
    in

    let trait_name = G.IdQualified (map_qualified_name env trait) in

    let _, body, _ = body in
    let body = List_.map (map_declaration env) body in

    ( entity,
      G.OtherDef (("Impl", token env impl), [ G.Name trait_name; G.Ss body ]) )
  in

  let map_impl_base (x : CST.impl_base) : G.definition =
    let attributes, impl, name, tparams, body = x in

    let entity : G.entity =
      {
        name = map_name_to_entity_name env name;
        attrs = map_attributes env attributes;
        tparams = Option.map (map_type_parameters env) tparams;
      }
    in

    let _, body, _ = body in
    let body = List_.map (map_declaration env) body in

    (entity, G.OtherDef (("Impl", token env impl), [ G.Ss body ]))
  in

  match x with
  | `Impl_base x -> map_impl_base x
  | `Impl_trait x -> map_impl_trait x

let map_source_file (env : env) (x : CST.source_file) : G.any =
  match x with
  | `Rep_choice_import_decl x -> G.Pr (List_.map (map_declaration env) x)
  | `Semg_exp (_, x) -> G.E (map_expression env x)
  | `Semg_stmt (_, x) -> G.Pr (List_.map (map_statement env) x)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_cairo.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = Target } in
      match map_source_file env cst with
      | G.Pr xs -> xs
      | _ -> failwith "not a program")

let parse_expression_or_source_file str =
  let res = Tree_sitter_cairo.Parse.string str in
  match res.errors with
  | [] -> res
  | _ -> (
      let expr_str = "__SEMGREP_EXPRESSION " ^ str in
      let expr_res = Tree_sitter_cairo.Parse.string expr_str in
      match expr_res.errors with
      | [] -> expr_res
      | _ ->
          let stmt_str = "__SEMGREP_STATEMENT " ^ str in
          Tree_sitter_cairo.Parse.string stmt_str)

let parse_pattern str =
  H.wrap_parser
    (fun () -> parse_expression_or_source_file str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env =
        { H.file; conv = H.line_col_to_pos_pattern str; extra = Pattern }
      in
      map_source_file env cst)
