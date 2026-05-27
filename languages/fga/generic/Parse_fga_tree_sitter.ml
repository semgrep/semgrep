(* Alex Useche (hex0punk)
 *
 * Copyright (c) 2026 Alex Useche (hex0punk)
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

open Fpath_.Operators
module CST = Tree_sitter_fga.CST
module H = Parse_tree_sitter_helpers
open AST_generic
module G = AST_generic
module H2 = AST_generic_helpers

type context = Program | Pattern
type env = context H.env

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket

(** We do this to be able to support different sorts of type references in direct relationships *)
let map_type_reference (env : env) (x : CST.anon_choice_id_6cee6b4) : G.expr =
  match x with
  | `Id tok ->
      (* Simple identifier: [user] *)
      let id = str env tok in
      N (H2.name_of_id id) |> G.e
  | `Rela_ref (v1, _hash, v3) ->
      (* Relation reference: [group#member] *)
      let type_id = str env v1 in
      let rel_id = str env v3 in
      (* Call: RelationRef(type, relation) *)
      let func = N (H2.name_of_id ("RelationRef", fake "RelationRef")) |> G.e in
      Call
        ( func,
          fb
            [
              Arg (N (H2.name_of_id type_id) |> G.e);
              Arg (N (H2.name_of_id rel_id) |> G.e);
            ] )
      |> G.e
  | `All (v1, _colonstar) ->
      (* [user:*] *)
      let type_id = str env v1 in
      (* Wildcard(type) *)
      let func = N (H2.name_of_id ("Wildcard", fake "Wildcard")) |> G.e in
      Call (func, fb [ Arg (N (H2.name_of_id type_id) |> G.e) ]) |> G.e
  | `Semg_meta tok ->
      let id = str env tok in
      N (H2.name_of_id id) |> G.e

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)

let map_boolean_literal (env : env) (x : CST.boolean_literal) =
  match x with
  | `True tok -> Bool (true, token env tok)
  | `False tok -> Bool (false, token env tok)

let map_number_literal (env : env) (x : CST.number_literal) =
  match x with
  | `Float_lit tok ->
      let s, t = str env tok in
      Float (float_of_string_opt s, t)
  | `Int_lit tok ->
      let s, t = str env tok in
      Int (Parsed_int.parse (s, t))
  | `Uint_lit tok ->
      let s, t = str env tok in
      Int (Parsed_int.parse (s, t))

let map_operator (env : env) (x : CST.operator) =
  match x with
  | `Or tok -> (Or, token env tok)
  | `And tok -> (And, token env tok)
  | `ButS tok ->
      let t = token env tok in
      (Minus, t)
(* "but not" mapped to Minus for now *)

let map_comparative_operator (env : env) (x : CST.comparative_operator) =
  match x with
  | `EQEQ tok -> (Eq, token env tok)
  | `BANGEQ tok -> (NotEq, token env tok)
  | `LT tok -> (Lt, token env tok)
  | `LTEQ tok -> (LtE, token env tok)
  | `GT tok -> (Gt, token env tok)
  | `GTEQ tok -> (GtE, token env tok)

let map_relation_identifier (env : env) (x : CST.anon_choice_id_096b091) :
    G.expr =
  match x with
  | `Id tok ->
      let id = str env tok in
      N (H2.name_of_id id) |> G.e
  | `Indi_rela (v1, _from_tok, v3) ->
      let rel_id = str env v1 in
      let type_id = str env v3 in
      let from_name = H2.name_of_id ("from", fake "from") in
      Call
        ( N from_name |> G.e,
          fb
            [
              Arg (N (H2.name_of_id rel_id) |> G.e);
              Arg (N (H2.name_of_id type_id) |> G.e);
            ] )
      |> G.e

let build_operator_chain (env : env) (base_expr : G.expr)
    (ops_and_ids : (CST.operator * CST.anon_choice_id_096b091) list) : G.expr =
  List.fold_left
    (fun acc_expr (op, id) ->
      let op_kind, op_tok = map_operator env op in
      let id_expr = map_relation_identifier env id in
      G.opcall (op_kind, op_tok) [ acc_expr; id_expr ])
    base_expr ops_and_ids

let rec map_expression (env : env) (x : CST.expression) : G.expr =
  match x with
  | `Num_lit x -> L (map_number_literal env x) |> G.e
  | `Bool_lit x -> L (map_boolean_literal env x) |> G.e
  | `Str_lit tok ->
      let s, t = str env tok in
      L (String (fb (s, t))) |> G.e
  | `Null_lit tok ->
      let t = token env tok in
      L (Null t) |> G.e
  | `Bin_exp x -> map_binary_expression env x
  | `Sele_exp (v1, _v2, v3) ->
      let id1 = str env v1 in
      let id2 = str env v3 in
      (* Selector like "context.user" *)
      let name = H2.name_of_ids [ id1; id2 ] in
      N name |> G.e
  | `Call_exp (v1, v2) ->
      let func =
        match v1 with
        | `Sele_exp (v1, _v2, v3) ->
            let id1 = str env v1 in
            let id2 = str env v3 in
            let name = H2.name_of_ids [ id1; id2 ] in
            N name |> G.e
        | `Id tok ->
            let id = str env tok in
            N (H2.name_of_id id) |> G.e
      in
      let args = map_argument_list env v2 in
      Call (func, args) |> G.e
  | `Id tok ->
      let id = str env tok in
      N (H2.name_of_id id) |> G.e
  | `Semg_ellips tok ->
      let t = token env tok in
      Ellipsis t |> G.e
  | `Semg_meta tok ->
      let id = str env tok in
      N (H2.name_of_id id) |> G.e

and map_binary_expression (env : env) (x : CST.binary_expression) : G.expr =
  match x with
  | `Exp_choice_STAR_exp (v1, v2, v3) ->
      let e1 = map_expression env v1 in
      let op, tok =
        match v2 with
        | `STAR tok -> (Mult, token env tok)
        | `SLASH tok -> (Div, token env tok)
        | `PERC tok -> (Mod, token env tok)
        | `LTLT tok -> (LSL, token env tok)
        | `GTGT tok -> (ASR, token env tok)
        | `AMP tok -> (BitAnd, token env tok)
        | `AMPHAT tok ->
            let t = token env tok in
            (BitXor, t)
        (* &^ operator - mapped to BitXor *)
      in
      let e3 = map_expression env v3 in
      G.opcall (op, tok) [ e1; e3 ]
  | `Exp_choice_PLUS_exp (v1, v2, v3) ->
      let e1 = map_expression env v1 in
      let op, tok =
        match v2 with
        | `PLUS tok -> (Plus, token env tok)
        | `DASH tok -> (Minus, token env tok)
        | `BAR tok -> (BitOr, token env tok)
        | `HAT tok -> (BitXor, token env tok)
      in
      let e3 = map_expression env v3 in
      G.opcall (op, tok) [ e1; e3 ]
  | `Exp_choice_EQEQ_exp (v1, v2, v3) ->
      let e1 = map_expression env v1 in
      let op, tok = map_comparative_operator env v2 in
      let e3 = map_expression env v3 in
      G.opcall (op, tok) [ e1; e3 ]
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let e1 = map_expression env v1 in
      let tok = token env v2 in
      let e3 = map_expression env v3 in
      G.opcall (And, tok) [ e1; e3 ]
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let e1 = map_expression env v1 in
      let tok = token env v2 in
      let e3 = map_expression env v3 in
      G.opcall (Or, tok) [ e1; e3 ]

and map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) :
    G.arguments =
  let l = token env v1 in
  let args =
    match v2 with
    | Some (v1, v2) ->
        let arg1 = Arg (map_expression env v1) in
        let rest =
          match v2 with
          | Some xs ->
              List.map (fun (_comma, expr) -> Arg (map_expression env expr)) xs
          | None -> []
        in
        arg1 :: rest
    | None -> []
  in
  let r = token env v3 in
  (l, args, r)

let map_anon_choice_id_684e964 (env : env) (x : CST.anon_choice_id_684e964) :
    G.ident =
  match x with
  | `Id tok -> str env tok
  | `Semg_meta tok -> str env tok

let map_direct_relationship (env : env) ((v1, v2, v3) : CST.direct_relationship)
    : G.expr =
  let lbracket = token env v1 in
  let rbracket = token env v3 in
  let items =
    match v2 with
    | `Choice_id_opt_cond_opt_rep_COMMA_choice_id_opt_cond
        (v1, _cond_opt, rest_opt) ->
        let first_item = map_type_reference env v1 in
        let rest_items =
          match rest_opt with
          | Some xs ->
              List.map
                (fun (_comma, type_ref, _cond_opt) ->
                  map_type_reference env type_ref)
                xs
          | None -> []
        in
        first_item :: rest_items
    | `Semg_ellips tok -> [ Ellipsis (token env tok) |> G.e ]
  in
  Container (Array, (lbracket, items, rbracket)) |> G.e

let map_relation_def (env : env) (x : CST.relation_def) : G.expr =
  match x with
  | `Direct_rela x -> map_direct_relationship env x
  | `Opt_direct_rela_op_choice_id_opt_rep_op_choice_id (direct_opt, v2, rest_opt)
    -> (
      let base_expr = map_relation_identifier env v2 in
      let expr_with_rest =
        match rest_opt with
        | Some ops_and_ids -> build_operator_chain env base_expr ops_and_ids
        | None -> base_expr
      in
      match direct_opt with
      | Some (direct, op) ->
          let direct_expr = map_direct_relationship env direct in
          let op_kind, op_tok = map_operator env op in
          G.opcall (op_kind, op_tok) [ direct_expr; expr_with_rest ]
      | None -> expr_with_rest)

let map_definition (env : env) ((v1, v2, v3, v4) : CST.definition) : G.stmt =
  let _define_tok = token env v1 in
  let name_id = map_anon_choice_id_684e964 env v2 in
  let _colon = token env v3 in
  let relation_expr = map_relation_def env v4 in
  (* Map to VarDef: define viewer: [user] becomes a variable definition *)
  let entity = G.basic_entity name_id in
  let vdef = { vinit = Some relation_expr; vtype = None; vtok = None } in
  DefStmt (entity, VarDef vdef) |> G.s

let map_condition_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.condition_declaration) : G.stmt =
  let _condition_tok = token env v1 in
  let name_id = str env v2 in
  let lparen = token env v3 in
  let params =
    match v4 with
    | Some (p1, rest_opt) ->
        let param1 =
          let param_name, _colon, param_type = p1 in
          let pname = map_anon_choice_id_684e964 env param_name in
          let ptype =
            match param_type with
            | `Type_id _x ->
                Some (G.ty_builtin ("string", fake "string")) (* simplified *)
            | `Semg_meta _tok -> None
          in
          G.param_of_id pname ?ptype
        in
        let rest_params =
          match rest_opt with
          | Some xs ->
              List.map
                (fun (_comma, (param_name, _colon, param_type)) ->
                  let pname = map_anon_choice_id_684e964 env param_name in
                  let ptype =
                    match param_type with
                    | `Type_id _x ->
                        Some (G.ty_builtin ("string", fake "string"))
                    | `Semg_meta _tok -> None
                  in
                  G.param_of_id pname ?ptype)
                xs
          | None -> []
        in
        param1 :: rest_params
    | None -> []
  in
  let rparen = token env v5 in
  let _lbrace, body_expr_raw, _rbrace = v6 in
  let body_expr = map_expression env body_expr_raw in
  (* Map to FuncDef: condition becomes a function *)
  let entity = G.basic_entity name_id in
  let fdef =
    {
      fkind = (G.Function, _condition_tok);
      fparams = (lparen, List.map (fun p -> G.Param p) params, rparen);
      frettype = None;
      fbody = G.FBExpr body_expr;
      (* Empty body for now *)
    }
  in
  DefStmt (entity, FuncDef fdef) |> G.s

let map_type_declaration (env : env)
    ((v1, v2, v3, v4, v5) : CST.type_declaration) : G.stmt =
  let _extend_opt =
    match v1 with
    | Some tok -> Some (token env tok)
    | None -> None
  in
  let type_tok = token env v2 in
  let type_name = map_anon_choice_id_684e964 env v3 in
  let _newline = token env v4 in
  let relations =
    match v5 with
    | Some (_relations_tok, defs) ->
        List.map
          (fun x ->
            match x with
            | `Defi def ->
                let stmt = map_definition env def in
                F stmt
            | `Semg_ellips tok ->
                let t = token env tok in
                G.field_ellipsis t)
          defs
    | None -> []
  in
  (* Map to ClassDef: type document becomes a class with relations as fields *)
  let entity = G.basic_entity type_name in
  let cdef =
    {
      ckind = (G.Class, type_tok);
      cextends = [];
      cimplements = [];
      cmixins = [];
      cparams = fb [];
      cbody = fb relations;
    }
  in
  DefStmt (entity, ClassDef cdef) |> G.s

let map_source_file (env : env) (x : CST.source_file) : G.any =
  match x with
  | `Proj_file (_schema, _contents) ->
      (* Project files (.openfga.yaml) - return empty program *)
      Pr []
  | `Module_file (header, decls) ->
      let _header =
        match header with
        | `Model (_model_tok, _newline, _schema) -> ()
        | `Module (_module_tok, _name) -> ()
      in
      let stmts =
        List.map
          (fun x ->
            match x with
            | `Type_decl td -> map_type_declaration env td
            | `Cond_decl cd -> map_condition_declaration env cd
            | `Semg_ellips tok ->
                let t = token env tok in
                ExprStmt (Ellipsis t |> G.e, t) |> G.s)
          decls
      in
      Pr stmts

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_expression_or_source_file = Tree_sitter_fga.Parse.string

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_fga.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = Program } in
      match map_source_file env cst with
      | G.Pr xs -> xs
      | _ -> failwith "not a program")

let parse_pattern str =
  H.wrap_parser
    (fun () -> parse_expression_or_source_file str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env =
        { H.file; conv = H.line_col_to_pos_pattern str; extra = Pattern }
      in
      map_source_file env cst)
