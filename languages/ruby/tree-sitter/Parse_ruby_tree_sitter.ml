(* Yoann Padioleau
 *
 * Copyright (C) 2020 Semgrep Inc.
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
open Fpath_.Operators
open Either_
module AST = Ast_ruby
module CST = Tree_sitter_ruby.CST
module Boilerplate = Tree_sitter_ruby.Boilerplate
open Ast_ruby
module G = AST_generic
module H = Parse_tree_sitter_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ruby parser using tree-sitter-lang/semgrep-ruby and converting
 * to ../ast/ast_ruby.ml
 *
 * The resulting AST can then be converted to the generic AST by using
 * ruby_to_generic.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type context = Program | Pattern
type env = context H.env

let fb = Tok.unsafe_fake_bracket

let list_to_maybe_tuple = function
  | [] -> raise Impossible
  | [ x ] -> x
  | xs -> Tuple xs

let mk_string_kind (t1, xs, t2) =
  match (Tok.content_of_tok t1, xs) with
  | "'", [] -> Single ("", Tok.combine_toks t1 [ t2 ])
  | "'", [ StrChars (s, t) ] -> Single (s, Tok.combine_toks t1 [ t; t2 ])
  | _ -> Double (t1, xs, t2)

let if_in_pattern (env : env) x =
  match env.extra with
  | Program -> raise Parsing.Parse_error
  | Pattern -> x

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-ruby/Boilerplate.ml *)

let token2 env x = H.token env x
let str env x = H.str env x
let false_ (env : env) (tok : CST.false_) : bool wrap = (false, token2 env tok)
let true_ (env : env) (tok : CST.true_) : bool wrap = (true, token2 env tok)
let nil (env : env) (tok : CST.nil) : tok = token2 env tok

let operator (env : env) (x : CST.operator) =
  match x with
  | `DOTDOT tok -> (Left Op_DOT2, token2 env tok)
  | `BAR tok -> (Left Op_BOR, token2 env tok)
  | `HAT tok -> (Left Op_XOR, token2 env tok)
  | `AMP tok -> (Left Op_BAND, token2 env tok)
  | `LTEQGT tok -> (Left Op_CMP, token2 env tok)
  | `EQEQ tok -> (Left Op_EQ, token2 env tok)
  | `EQEQEQ tok -> (Left Op_EQQ, token2 env tok)
  | `EQTILDE tok -> (Left Op_MATCH, token2 env tok)
  | `GT tok -> (Left Op_GT, token2 env tok)
  | `GTEQ tok -> (Left Op_GEQ, token2 env tok)
  | `LT tok -> (Left Op_LT, token2 env tok)
  | `LTEQ tok -> (Left Op_LEQ, token2 env tok)
  | `PLUS tok -> (Left Op_PLUS, token2 env tok)
  | `DASH tok -> (Left Op_MINUS, token2 env tok)
  | `STAR tok -> (Left Op_TIMES, token2 env tok)
  | `SLASH tok -> (Left Op_DIV, token2 env tok)
  | `PERC tok -> (Left Op_REM, token2 env tok)
  | `BANGTILDE tok -> (Left Op_NMATCH, token2 env tok)
  | `STARSTAR tok -> (Left Op_POW, token2 env tok)
  | `LTLT tok -> (Left Op_LSHIFT, token2 env tok)
  | `GTGT tok -> (Left Op_RSHIFT, token2 env tok)
  | `LBRACKRBRACK tok -> (Left Op_AREF, token2 env tok)
  | `LBRACKRBRACKEQ tok -> (Left Op_ASET, token2 env tok)
  | `PLUSAT tok -> (Right Op_UPlus, token2 env tok)
  | `DASHAT tok -> (Right Op_UMinus, token2 env tok)
  | `TILDEAT tok ->
      (* "~@" *)
      (Right Op_UTilde, token2 env tok)
  | `TILDE tok -> (Right Op_UTilde, token2 env tok)
  | `BANG tok -> (Right Op_UBang, token2 env tok)
  (* TODO *)
  | `BQUOT _tok -> failwith "Op_BQUOT???"

let int_or_float (env : env) (x : CST.int_or_float) =
  match x with
  | `Int tok ->
      str env tok
      (* pattern 0[bB][01](_?[01])*|0[oO]?[0-7](_?[0-7])*|(0[dD])?\d(_?\d)*|0x[0-9a-fA-F](_?[0-9a-fA-F])* *)
  | `Float tok -> str env tok

(* pattern \d(_?\d)*(\.\d)?(_?\d)*([eE][\+-]?\d(_?\d)*\
   )? *)

let call_operator (env : env) (x : CST.call_operator) =
  match x with
  | `DOT tok -> (* "." *) token2 env tok
  | `AMPDOT tok -> (* "&." *) token2 env tok
  | `Imm_tok_colo tok -> (* "::" *) token2 env tok

let terminator (_env : env) (x : CST.terminator) : unit =
  match x with
  | `Line_brk _tok -> ()
  | `SEMI _tok -> ()

let nonlocal_variable (env : env) (x : CST.nonlocal_variable) : AST.variable =
  match x with
  | `Inst_var tok -> (str env tok, ID_Instance)
  | `Class_var tok -> (str env tok, ID_Class)
  | `Global_var tok -> (str env tok, ID_Global)

let constant (env : env) (x : CST.constant) =
  match x with
  | `Semg_meta tok
  | `Tok_pat_562b724_pat_f7bc484 tok ->
      (str env tok, ID_Uppercase)

let variable (env : env) (x : CST.variable) =
  match x with
  (* TODO: move this to variable type *)
  | `Self tok -> Id (str env tok, ID_Self)
  | `Super tok -> Id (str env tok, ID_Super)
  | `Nonl_var x -> Id (nonlocal_variable env x)
  | `Id tok -> Id (str env tok, ID_Lowercase)
  | `Cst x -> Id (constant env x)

let rec statements (env : env) (x : CST.statements) : AST.stmts =
  match x with
  | `Rep1_choice_stmt_term_opt_stmt (v1, v2) ->
      let v1 =
        List.concat_map
          (fun x ->
            match x with
            | `Stmt_term (v1, v2) ->
                let v1 = statement env v1 in
                let _v2 = terminator env v2 in
                [ v1 ]
            (* TODO? use EmptyStmt in generic AST? *)
            | `Empty_stmt _tok -> [])
          v1
      in
      let v2 =
        match v2 with
        | Some x -> [ statement env x ]
        | None -> []
      in
      v1 @ v2
  | `Stmt x -> [ statement env x ]

and statement (env : env) (x : CST.statement) :
    AST.expr (* TODO AST.stmt at some point *) =
  match x with
  | `Undef (v1, v2, v3) ->
      let v1 = token2 env v1 in
      let v2 = method_name env v2 in
      let v3 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = token2 env v1 in
            let v2 = method_name env v2 in
            v2)
          v3
      in
      D (Undef (v1, v2 :: v3))
  | `Alias (v1, v2, v3) ->
      let v1 = token2 env v1 in
      let v2 = method_name env v2 in
      let v3 = method_name env v3 in
      D (Alias (v1, v2, v3))
  | `If_modi (v1, v2, v3) ->
      let v1 = statement env v1 in
      let v2 = token2 env v2 in
      let v3 = expression env v3 in
      S (If (v2, v3, [ v1 ], None))
  | `Unless_modi (v1, v2, v3) ->
      let v1 = statement env v1 in
      let v2 = token2 env v2 in
      let v3 = expression env v3 in
      S (Unless (v2, v3, [ v1 ], None))
  | `While_modi (v1, v2, v3) ->
      let v1 = statement env v1 in
      let v2 = token2 env v2 in
      let v3 = expression env v3 in
      let b = true (* ?? *) in
      S (While (v2, b, v3, [ v1 ]))
  | `Until_modi (v1, v2, v3) ->
      let v1 = statement env v1 in
      let v2 = token2 env v2 in
      let v3 = expression env v3 in
      S (Until (v2, true, v3, [ v1 ]))
  | `Rescue_modi (v1, v2, v3) ->
      let v1 = statement env v1 in
      let v2 = token2 env v2 in
      let v3 = expression env v3 in
      S
        (ExnBlock
           {
             body_exprs = [ v1 ];
             rescue_exprs = [ (v2, [], None, [ v3 ]) ];
             ensure_expr = None;
             else_expr = None;
           })
  | `Begin_blk (v1, v2, v3, v4) ->
      let v1 = token2 env v1 in
      let v2 = token2 env v2 in
      let v3 =
        match v3 with
        | Some x -> statements env x
        | None -> []
      in
      let v4 = token2 env v4 in
      D (BeginBlock (v1, (v2, v3, v4)))
  | `End_blk (v1, v2, v3, v4) ->
      let v1 = token2 env v1 in
      let v2 = token2 env v2 in
      let v3 =
        match v3 with
        | Some x -> statements env x
        | None -> []
      in
      let v4 = token2 env v4 in
      D (EndBlock (v1, (v2, v3, v4)))
  | `Exp x -> expression env x

and body_expr (env : env) ((v1, v2) : CST.body_expr) =
  let _v1 = (* "=" *) token2 env v1 in
  let v2 = arg_rhs env v2 in
  { empty_body_exn with body_exprs = [ v2 ] }

and method_rest (env : env) ((v1, v2) : CST.method_rest) =
  let name = method_name env v1 in
  match v2 with
  | `Body_expr x -> (name, [], body_expr env x)
  | `Params_choice_opt_term_opt_body_stmt_end (v1, v2) ->
      let _, params, _ = parameters env v1 in
      let body =
        match v2 with
        | `Opt_term_opt_body_stmt_end (v1, v2, v3) ->
            let _v1 =
              match v1 with
              | Some x -> Some (terminator env x)
              | None -> None
            in
            let v2 =
              match v2 with
              | Some x -> body_statement env x
              | None -> empty_body_exn
            in
            let _v3 = (* "end" *) token2 env v3 in
            v2
        | `Body_expr x -> body_expr env x
      in
      (name, params, body)
  | `Opt_bare_params_term_opt_body_stmt_end (v1, v2, v3, v4) ->
      let params =
        match v1 with
        | Some x -> bare_parameters env x
        | None -> []
      in
      let _v2 = terminator env v2 in
      let body =
        match v3 with
        | Some x -> body_statement env x
        | None -> empty_body_exn
      in
      let _v4 = (* "end" *) token2 env v4 in
      (name, params, body)

and parameters (env : env) ((v1, v2, v3) : CST.parameters) :
    AST.formal_param list bracket =
  let lp = token2 env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = formal_parameter env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = token2 env v1 in
              let v2 = formal_parameter env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let rp = token2 env v3 in
  (lp, v2, rp)

and bare_parameters (env : env) ((v1, v2) : CST.bare_parameters) :
    AST.formal_param list =
  let v1 = simple_formal_parameter env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token2 env v1 in
        let v2 = formal_parameter env v2 in
        v2)
      v2
  in
  v1 :: v2

and block_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.block_parameters) :
    AST.formal_param list bracket =
  let pipe1 = token2 env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = formal_parameter env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = token2 env v1 in
              let v2 = formal_parameter env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 =
    match v3 with
    | Some _tok -> ()
    | None -> ()
  in
  let _v4SEMICOLONPARAMS =
    match v4 with
    | Some (v1, v2, v3) ->
        let _v1 = token2 env v1 in
        let v2 = str env v2 in
        let v3 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = token2 env v1 in
              let v2 = str env v2 in
              v2)
            v3
        in
        v2 :: v3
    | None -> []
  in
  let pipe2 = token2 env v5 in
  (pipe1, v2, pipe2)

and formal_parameter (env : env) (x : CST.formal_parameter) : AST.formal_param =
  match x with
  | `Simple_formal_param x -> simple_formal_parameter env x
  | `Params x ->
      let lp, xs, rp = parameters env x in
      Formal_tuple (lp, xs, rp)

and simple_formal_parameter (env : env) (x : CST.simple_formal_parameter) :
    AST.formal_param =
  match x with
  | `Id tok ->
      let id = str env tok in
      Formal_id id
  | `Splat_param (v1, v2) -> (
      let v1 = token2 env v1 in
      match v2 with
      | Some tok ->
          let id = str env tok in
          Formal_star (v1, id)
      | None -> Formal_rest v1)
  | `Hash_splat_param (v1, v2) ->
      let v1 = token2 env v1 in
      let v2 =
        match v2 with
        | Some tok -> Some (str env tok)
        | None -> None
      in
      Formal_hash_splat (v1, v2)
  | `Hash_splat_nil (v1, v2) ->
      let v1 = (* "**" *) token2 env v1 in
      let v2 = (* "nil" *) str env v2 in
      (* https://stackoverflow.com/questions/34125769/double-splat-on-nil *)
      (* Shouldn't be possible, it seems, but let's just allow it. *)
      Formal_hash_splat (v1, Some v2)
  | `Forw_param tok -> (
      let x = (* "..." *) token2 env tok in
      match env.extra with
      | Program -> Formal_fwd x
      | Pattern -> ParamEllipsis x)
  | `Blk_param (v1, v2) ->
      let v1 = token2 env v1 in
      let v2 =
        match v2 with
        | Some id -> Some (str env id)
        | None -> None
      in
      Formal_amp (v1, v2)
  | `Kw_param (v1, v2, v3) ->
      let v1 = str env v1 in
      let v2 = token2 env v2 in
      let v3 =
        match v3 with
        | Some x -> Some (arg env x)
        | None -> None
      in
      Formal_kwd (v1, v2, v3)
  | `Opt_param (v1, v2, v3) ->
      let v1 = str env v1 in
      let v2 = token2 env v2 in
      let v3 = arg env v3 in
      Formal_default (v1, v2, v3)

and superclass (env : env) ((v1, v2) : CST.superclass) =
  let v1 = token2 env v1 in
  let v2 = expression env v2 in
  (v1, v2)

and in_ (env : env) ((v1, v2) : CST.in_) =
  let v1 = token2 env v1 in
  let v2 = arg env v2 in
  (v1, v2)

and when_ (env : env) ((v1, v2, v3, v4) : CST.when_) =
  let twhen = token2 env v1 in
  let v2 = pattern env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _tcomma = token2 env v1 in
        let v2 = pattern env v2 in
        v2)
      v3
  in
  let v4 =
    match v4 with
    | `Term x ->
        let _ = terminator env x in
        []
    | `Then x -> then_ env x
  in
  (twhen, v2 :: v3, v4)

and pattern (env : env) (x : CST.pattern) : AST.pattern =
  match x with
  | `Arg x ->
      (* TODO: some normalization? *)
      PatExpr (arg env x)
  | `Splat_arg x -> PatExpr (splat_argument env x)

and elsif (env : env) ((v1, v2, v3, v4) : CST.elsif) : AST.tok * AST.stmt =
  let v1 = token2 env v1 in
  let v2 = statement env v2 in
  let v3 =
    match v3 with
    | `Term x ->
        let _ = terminator env x in
        []
    | `Then x -> then_ env x
  in
  let v4 =
    match v4 with
    | Some x ->
        Some
          (match x with
          | `Else x -> else_ env x
          | `Elsif x ->
              let t, s = elsif env x in
              (t, [ S s ]))
    | None -> None
  in
  (v1, If (v1, v2, v3, v4))

and else_ (env : env) ((v1, v2, v3) : CST.else_) : AST.tok * AST.stmts =
  let v1 = token2 env v1 in
  let _v2 =
    match v2 with
    | Some x -> terminator env x
    | None -> ()
  in
  let v3 =
    match v3 with
    | Some x -> statements env x
    | None -> []
  in
  (v1, v3)

and then_ (env : env) (x : CST.then_) : AST.stmts =
  match x with
  | `Term_stmts (v1, v2) ->
      let _v1 = terminator env v1 in
      let v2 = statements env v2 in
      v2
  | `Opt_term_then_opt_stmts (v1, v2, v3) ->
      let _v1 =
        match v1 with
        | Some x -> terminator env x
        | None -> ()
      in
      let _v2 = token2 env v2 in
      let v3 =
        match v3 with
        | Some x -> statements env x
        | None -> []
      in
      v3

and ensure (env : env) ((v1, v2) : CST.ensure) =
  let v1 = token2 env v1 in
  let v2 =
    match v2 with
    | Some x -> statements env x
    | None -> []
  in
  (v1, v2)

and rescue (env : env) ((v1, v2, v3, v4) : CST.rescue) =
  let v1 = token2 env v1 in
  let v2 =
    match v2 with
    | Some x -> exceptions env x
    | None -> []
  in
  let v3 =
    match v3 with
    | Some x -> Some (exception_variable env x)
    | None -> None
  in
  let v4 =
    match v4 with
    | `Term x ->
        let _ = terminator env x in
        []
    | `Then x -> then_ env x
  in
  (v1, v2, v3, v4)

and exceptions (env : env) ((v1, v2) : CST.exceptions) : AST.expr list =
  let v1 =
    match v1 with
    | `Arg x -> arg env x
    | `Splat_arg x -> splat_argument env x
  in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token2 env v1 in
        let v2 =
          match v2 with
          | `Arg x -> arg env x
          | `Splat_arg x -> splat_argument env x
        in
        v2)
      v2
  in
  v1 :: v2

and exception_variable (env : env) ((v1, v2) : CST.exception_variable) =
  let v1 = token2 env v1 in
  let v2 = lhs env v2 in
  (v1, v2)

and map_anon_choice_rescue_d627f1b (env : env)
    (x : CST.anon_choice_rescue_d627f1b) =
  match x with
  | `Rescue x -> Either_.Left3 (rescue env x)
  | `Else x -> Either_.Middle3 (else_ env x)
  | `Ensure x -> Either_.Right3 (ensure env x)

and body_statement_ (env : env) (x : CST.body_statement_) : body_exn =
  let stmts, v2 =
    match x with
    | `Stmts_rep_choice_rescue (v1, v2) -> (statements env v1, v2)
    | `Opt_stmts_rep1_choice_rescue (v1, v2) -> (
        match v1 with
        | None -> ([], v2)
        | Some v1 -> (statements env v1, v2))
  in
  let rescue_exprs, else_expr, ensure_expr =
    Either_.partition_either3 (map_anon_choice_rescue_d627f1b env) v2
  in
  let ensure_expr =
    match ensure_expr with
    | [] -> None
    | [ x ] -> Some x
    | x :: _ -> Some x
    (* TODO: weird, should have only one no? *)
  in
  let else_expr =
    match else_expr with
    | [] -> None
    | [ x ] -> Some x
    | x :: _ -> Some x
    (* TODO: weird, should have only one no? *)
  in
  { body_exprs = stmts; rescue_exprs; else_expr; ensure_expr }

and body_statement (env : env) (x : CST.body_statement) : AST.body_exn =
  body_statement_ env x

and body_statement_to_exprs (env : env) (x : CST.body_statement) : expr list =
  match body_statement_ env x with
  | { rescue_exprs = []; else_expr = None; ensure_expr = None; body_exprs } ->
      body_exprs
  | other -> [ S (ExnBlock other) ]

and identifier_suffix (env : env) (x : CST.identifier_suffix) =
  match x with
  | `Tok_pat_3fee85b_pat_f7bc484_pat_38b534e x -> str env x
  | `Id_suffix_ tok -> (* identifier_suffix_ *) str env tok

and constant_suffix (env : env) (x : CST.constant_suffix) =
  match x with
  | `Tok_pat_562b724_pat_f7bc484_pat_38b534e x -> str env x
  | `Cst_suffix_ tok -> (* constant_suffix_ *) str env tok

and function_identifier (env : env) (x : CST.function_identifier) =
  match x with
  | `Id_suffix x -> identifier_suffix env x
  | `Cst_suffix x -> constant_suffix env x

and pattern_expr (env : env) (x : CST.pattern_expr) =
  match x with
  | `As_pat (v1, v2, v3) ->
      let v1 = pattern_expr env v1 in
      let _v2 = (* "=>" *) token2 env v2 in
      let v3 = (* identifier *) str env v3 in
      PatAs (v1, v3)
  | `Pat_expr_alt x -> pattern_expr_alt env x

and pattern_expr_alt (env : env) (x : CST.pattern_expr_alt) =
  match x with
  | `Alt_pat (v1, v2) ->
      let v1 = pattern_expr_basic env v1 in
      List_.fold_right
        (fun (v1, pat) acc ->
          let _v1 = (* "|" *) token2 env v1 in
          let v2 = pattern_expr_basic env pat in
          PatDisj (v2, acc))
        v2 v1
  | `Pat_expr_basic x -> pattern_expr_basic env x

and pattern_expr_basic (env : env) (x : CST.pattern_expr_basic) =
  match x with
  | `Pat_value x -> pattern_value env x
  | `Id tok ->
      (* identifier *)
      PatId (str env tok, ID_Lowercase)
  | `Array_pat x -> (
      let qual, ps = array_pattern env x in
      match qual with
      | None -> PatList ps
      | Some qual ->
          (* Could also make type where PatConstructor takes in patlist_arg list *)
          PatConstructor (qual, [ PatList ps ]))
  | `Find_pat x -> (
      let qual, ps = find_pattern env x in
      match qual with
      | None -> PatList ps
      | Some qual ->
          (* Could also make type where PatConstructor takes in patlist_arg list *)
          PatConstructor (qual, [ PatList ps ]))
  | `Hash_pat x -> (
      let qual, ps = hash_pattern env x in
      match qual with
      | None -> PatList ps
      | Some qual ->
          (* Could also make type where PatConstructor takes in patlist_arg list *)
          PatConstructor (qual, [ PatList ps ]))
  | `Paren_pat (v1, v2, v3) ->
      let _v1 = (* "(" *) token2 env v1 in
      let v2 = pattern_expr env v2 in
      let _v3 = (* ")" *) token2 env v3 in
      v2

and hash_pattern (env : env) (x : CST.hash_pattern) :
    qualified option * patlist_arg list bracket =
  match x with
  | `LCURL_opt_hash_pat_body_RCURL (v1, v2, v3) ->
      let v1 = (* "{" *) token2 env v1 in
      let v2 =
        match v2 with
        | Some x -> hash_pattern_body env x
        | None -> []
      in
      let v3 = (* "}" *) token2 env v3 in
      (None, (v1, v2, v3))
  | `Pat_cst_imm_tok_lbrack_hash_pat_body_RBRACK (v1, v2, v3, v4) ->
      let v1 = pattern_constant env v1 in
      let v2 = (* "[" *) token2 env v2 in
      let v3 = hash_pattern_body env v3 in
      let v4 = (* "]" *) token2 env v4 in
      (Some v1, (v2, v3, v4))
  | `Pat_cst_imm_tok_lpar_hash_pat_body_RPAR (v1, v2, v3, v4) ->
      let v1 = pattern_constant env v1 in
      let v2 = (* "(" *) token2 env v2 in
      let v3 = hash_pattern_body env v3 in
      let v4 = (* ")" *) token2 env v4 in
      (Some v1, (v2, v3, v4))

and hash_pattern_body (env : env) (x : CST.hash_pattern_body) : patlist_arg list
    =
  match x with
  | `Kw_pat_rep_COMMA_kw_pat_opt_COMMA (v1, v2, v3) ->
      let v1 = keyword_pattern env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token2 env v1 in
            let v2 = keyword_pattern env v2 in
            v2)
          v2
      in
      let _v3 =
        match v3 with
        | Some tok -> Some ((* "," *) token2 env tok)
        | None -> None
      in
      v1 :: v2
  | `Kw_pat_rep_COMMA_kw_pat_COMMA_hash_pat_any_rest (v1, v2, v3, v4) ->
      let v1 = keyword_pattern env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token2 env v1 in
            let v2 = keyword_pattern env v2 in
            v2)
          v2
      in
      let _v3 = (* "," *) token2 env v3 in
      let v4 = hash_pattern_any_rest env v4 in
      (v1 :: v2) @ [ v4 ]
  | `Hash_pat_any_rest x -> [ hash_pattern_any_rest env x ]

and hash_pattern_any_rest (env : env) (x : CST.hash_pattern_any_rest) =
  match x with
  | `Hash_splat_param x -> hash_splat_parameter env x
  | `Hash_splat_nil x -> hash_splat_nil env x

and hash_splat_nil (env : env) ((v1, v2) : CST.hash_splat_nil) =
  let v1 = (* "**" *) token2 env v1 in
  let v2 = (* "nil" *) str env v2 in
  PArgSplat (v1, Some v2)

and hash_splat_parameter (env : env) ((v1, v2) : CST.hash_splat_parameter) =
  let v1 = (* "**" *) token2 env v1 in
  let v2 =
    match v2 with
    | Some tok -> Some ((* identifier *) str env tok)
    | None -> None
  in
  PArgSplat (v1, v2)

and keyword_pattern (env : env) (x : CST.keyword_pattern) =
  match x with
  | `Semg_ellips tok -> if_in_pattern env (PArgEllipsis (token2 env tok))
  | `Choice_id_imm_tok_colon_opt_pat_expr (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Id tok ->
            (* identifier *)
            PatId (str env tok, ID_Lowercase)
        | `Cst x -> PatId (constant env x)
        | `Id_suffix x -> PatId (identifier_suffix env x, ID_Lowercase)
        | `Cst_suffix x -> PatId (constant_suffix env x, ID_Uppercase)
        | `Str x -> PatLiteral (String (mk_string_kind (string_ env x)))
      in
      let v2 = (* ":" *) token2 env v2 in
      let v3 =
        match v3 with
        | Some x -> Some (pattern_expr env x)
        | None -> None
      in
      PArgKeyVal (v1, v2, v3)

and array_pattern (env : env) (x : CST.array_pattern) :
    qualified option * patlist_arg list bracket =
  match x with
  | `LBRACK_opt_array_pat_body_RBRACK (v1, v2, v3) ->
      let l = (* "[" *) token2 env v1 in
      let v2 =
        match v2 with
        | Some x -> array_pattern_body env x
        | None -> []
      in
      let r = (* "]" *) token2 env v3 in
      (None, (l, v2, r))
  | `Pat_cst_imm_tok_lbrack_opt_array_pat_body_RBRACK (v1, v2, v3, v4) ->
      let v1 = pattern_constant env v1 in
      let v2 = (* "[" *) token2 env v2 in
      let v3 =
        match v3 with
        | Some x -> array_pattern_body env x
        | None -> []
      in
      let v4 = (* "]" *) token2 env v4 in
      (Some v1, (v2, v3, v4))
  | `Pat_cst_imm_tok_lpar_opt_array_pat_body_RPAR (v1, v2, v3, v4) ->
      let v1 = pattern_constant env v1 in
      let v2 = (* "(" *) token2 env v2 in
      let v3 =
        match v3 with
        | Some x -> array_pattern_body env x
        | None -> []
      in
      let v4 = (* ")" *) token2 env v4 in
      (Some v1, (v2, v3, v4))

and array_pattern_body (env : env) (x : CST.array_pattern_body) =
  match x with
  | `Pat_expr x -> [ PArgPat (pattern_expr env x) ]
  | `Array_pat_n x -> array_pattern_n env x

and array_pattern_n (env : env) (x : CST.array_pattern_n) : patlist_arg list =
  match x with
  | `Pat_expr_COMMA (v1, v2) ->
      let v1 = pattern_expr env v1 in
      let _v2 = (* "," *) token2 env v2 in
      [ PArgPat v1 ]
  | `Pat_expr_COMMA_choice_pat_expr (v1, v2, v3) ->
      let v1 = pattern_expr env v1 in
      let _v2 = (* "," *) token2 env v2 in
      let v3 = array_pattern_body env v3 in
      PArgPat v1 :: v3
  | `Splat_param_rep_COMMA_pat_expr (v1, v2) ->
      let v1 = splat_parameter env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token2 env v1 in
            let v2 = pattern_expr env v2 in
            PArgPat v2)
          v2
      in
      v1 :: v2

and find_pattern (env : env) (x : CST.find_pattern) :
    qualified option * patlist_arg list bracket =
  match x with
  | `LBRACK_find_pat_body_RBRACK (v1, v2, v3) ->
      let l = (* "[" *) token2 env v1 in
      let v2 = find_pattern_body env v2 in
      let r = (* "]" *) token2 env v3 in
      (None, (l, v2, r))
  | `Pat_cst_imm_tok_lbrack_find_pat_body_RBRACK (v1, v2, v3, v4) ->
      let v1 = pattern_constant env v1 in
      let l = (* "[" *) token2 env v2 in
      let v3 = find_pattern_body env v3 in
      let r = (* "]" *) token2 env v4 in
      (Some v1, (l, v3, r))
  | `Pat_cst_imm_tok_lpar_find_pat_body_RPAR (v1, v2, v3, v4) ->
      let v1 = pattern_constant env v1 in
      let l = (* "[" *) token2 env v2 in
      let v3 = find_pattern_body env v3 in
      let r = (* ")" *) token2 env v4 in
      (Some v1, (l, v3, r))

and splat_parameter (env : env) ((v1, v2) : CST.splat_parameter) =
  let v1 = (* "*" *) token2 env v1 in
  let v2 =
    match v2 with
    | Some tok -> Some ((* identifier *) str env tok)
    | None -> None
  in
  PArgSplat (v1, v2)

and find_pattern_body (env : env) ((v1, v2, v3, v4) : CST.find_pattern_body) :
    patlist_arg list =
  let v1 = splat_parameter env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token2 env v1 in
        let v2 = pattern_expr env v2 in
        PArgPat v2)
      v2
  in
  let _v3 = (* "," *) token2 env v3 in
  let v4 = splat_parameter env v4 in
  (v1 :: v2) @ [ v4 ]

and pattern_constant (env : env) (x : CST.pattern_constant) : qualified =
  match x with
  | `Cst x -> [ constant env x ]
  | `Pat_cst_resol (v1, v2, v3) -> (
      let _v2 = (* "::" *) token2 env v2 in
      let v3 = (* constant *) constant env v3 in
      match v1 with
      | Some x ->
          let x = pattern_constant env x in
          x @ [ v3 ]
      | None -> [ v3 ])

and pattern_value (env : env) (x : CST.pattern_value) =
  match x with
  | `Pat_prim x -> PatExpr (pattern_primitive env x)
  | `Pat_range x -> PatExpr (pattern_range env x)
  | `Var_ref_pat (v1, v2) -> (
      let v1 = (* "^" *) token2 env v1 in
      match v2 with
      | `Id tok ->
          (* identifier *)
          PatPin (v1, Id (str env tok, ID_Lowercase))
      | `Nonl_var x -> PatPin (v1, Id (nonlocal_variable env x)))
  | `Exp_ref_pat (v1, v2, v3, v4) ->
      let v1 = (* "^" *) token2 env v1 in
      let _v2 = (* "(" *) token2 env v2 in
      let v3 = expression env v3 in
      let _v4 = (* ")" *) token2 env v4 in
      PatPin (v1, v3)
  | `Pat_cst x -> (
      match pattern_constant env x with
      | [ x ] -> PatId x
      | other -> PatConstructor (other, []))

and pattern_primitive (env : env) (x : CST.pattern_primitive) : expr =
  match x with
  | `Pat_lit x -> pattern_literal env x
  | `Pat_lambda x -> lambda env x

and pattern_range (env : env) (x : CST.pattern_range) : expr =
  match x with
  | `Pat_prim_choice_DOTDOT_pat_prim (v1, v2, v3) ->
      let v1 = pattern_primitive env v1 in
      let v2 = anon_choice_DOTDOT_ed078ec env v2 in
      let v3 = pattern_primitive env v3 in
      Binop (v1, v2, v3)
  | `Choice_DOTDOT_pat_prim (v1, v2) ->
      let v1 = anon_choice_DOTDOT_ed078ec env v1 in
      let v2 = pattern_primitive env v2 in
      let t = snd v1 in
      Binop (fake_nil t, v1, v2)
  | `Pat_prim_choice_DOTDOT (v1, v2) ->
      let v1 = pattern_primitive env v1 in
      let v2 = anon_choice_DOTDOT_ed078ec env v2 in
      let t = snd v2 in
      Binop (v1, v2, fake_nil t)

and subshell (env : env) ((v1, v2, v3) : CST.subshell) =
  let v1 = (* subshell_start *) token2 env v1 in
  let v2 =
    match v2 with
    | Some x -> literal_contents env x
    | None -> []
  in
  let v3 = (* string_end *) token2 env v3 in
  Literal (String (Tick (v1, v2, v3)))

and regex (env : env) ((v1, v2, v3) : CST.regex) =
  let v1 = (* regex_start *) token2 env v1 in
  let v2 =
    match v2 with
    | Some x -> literal_contents env x
    | None -> []
  in
  let v3 = (* string_end *) token2 env v3 in
  (v1, v2, v3)

and string_array (env : env) ((v1, v2, v3, v4, v5) : CST.string_array) =
  let v1 = token2 env v1 (* string_array_start *) in
  let _v2 =
    match v2 with
    | Some tok -> Some (token2 env tok) (* pattern \s+ *)
    | None -> None
  in
  let v3 =
    match v3 with
    | Some x -> anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e env x
    | None -> []
  in
  let _v4 =
    match v4 with
    | Some tok -> Some (token2 env tok)
    | None -> None
  in
  let v5 = token2 env v5 (* string_end *) in
  Literal (String (Double (v1, v3 |> List_.flatten, v5)))

and symbol_array (env : env) ((v1, v2, v3, v4, v5) : CST.symbol_array) =
  let v1 = token2 env v1 (* %i( *) in
  let _v2 =
    match v2 with
    | Some tok -> Some (token2 env tok)
    | None -> None
  in
  let v3 =
    match v3 with
    | Some x -> anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e env x
    | None -> []
  in
  let _v4 =
    match v4 with
    | Some tok -> Some (token2 env tok)
    | None -> None
  in
  let v5 = token2 env v5 (* ) *) in
  Atom (v1, AtomFromString (v1, v3 |> List_.flatten, v5))

and pattern_literal (env : env) (x : CST.pattern_literal) : expr =
  match x with
  | `Lit x -> literal env x
  | `Str x -> Literal (String (mk_string_kind (string_ env x)))
  | `Subs x -> subshell env x
  | `Here_begin tok ->
      (* heredoc_beginning *)
      Literal (String (Single (str env tok)))
  | `Regex x -> Literal (Regexp (regex env x, None))
  | `Str_array x -> string_array env x
  | `Symb_array x -> symbol_array env x
  | `Kw_var x -> keyword_variable env x

and keyword_variable (env : env) (x : CST.keyword_variable) =
  match x with
  | `Nil tok -> (* "nil" *) Id (str env tok, ID_Lowercase)
  | `Self tok -> (* "self" *) Id (str env tok, ID_Self)
  | `True tok -> (* "true" *) Literal (Bool (true, token2 env tok))
  | `False tok -> (* "false" *) Literal (Bool (false, token2 env tok))
  | `Line tok -> (* "__LINE__" *) Id (str env tok, ID_Lowercase)
  | `File tok -> (* "__FILE__" *) Id (str env tok, ID_Lowercase)
  | `Enco tok -> (* "__ENCODING__" *) Id (str env tok, ID_Lowercase)

and unary_lit (env : env) ((v1, v2) : CST.unary_literal) =
  let v1 =
    match v1 with
    | `Un_minus_num tok -> (U Op_UMinus, token2 env tok)
    | `PLUS tok -> (U Op_UPlus, token2 env tok)
  in
  let v2 = simple_numeric env v2 in
  Unary (v1, Literal v2)

and numeric (env : env) (x : CST.numeric) : expr =
  match x with
  | `Simple_nume x -> Literal (simple_numeric env x)
  | `Un_lit (v1, v2) -> unary_lit env (v1, v2)

and complex (env : env) (x : CST.complex) =
  match x with
  | `Int_or_float_imm_tok_i (v1, v2) ->
      let s1, t1 = int_or_float env v1 in
      let s2, t2 = str env v2 in
      (s1 ^ s2, Tok.combine_toks t1 [ t2 ])
  | `Int_or_float_imm_tok_ri (v1, v2) ->
      let s1, t1 = int_or_float env v1 in
      let s2, t2 = str env v2 in
      (s1 ^ s2, Tok.combine_toks t1 [ t2 ])

and simple_numeric (env : env) (x : CST.simple_numeric) =
  match x with
  | `Int tok ->
      (* pattern 0[bB][01](_?[01])*|0[oO]?[0-7](_?[0-7])*|(0[dD])?\d(_?\d)*|0[xX][0-9a-fA-F](_?[0-9a-fA-F])* *)
      Num (str env tok)
  | `Float tok ->
      (* pattern \d(_?\d)*(\.\d)?(_?\d)*([eE][\+-]?\d(_?\d)*\
         )? *)
      Float (str env tok)
  | `Comp x -> Complex (complex env x)
  | `Rati (v1, v2) ->
      let v1 = int_or_float env v1 in
      let v2 = token2 env v2 in
      Rational (v1, v2)

and literal (env : env) (x : CST.literal) : expr =
  match x with
  | `Simple_symb tok ->
      (* simple_symbol *)
      let s, t = str env tok in
      if String.starts_with ~prefix:":" s then
        let tcolon, tafter = Tok.split_tok_at_bytepos 1 t in
        let str = Tok.content_of_tok tafter in
        Atom (tcolon, AtomSimple (str, tafter))
      else Id ((s, t), ID_Lowercase)
  | `Deli_symb x -> Atom (delimited_symbol env x)
  | `Nume x -> numeric env x

and pattern_top_expr_body (env : env) (x : CST.pattern_top_expr_body) =
  match x with
  | `Pat_expr x -> pattern_expr env x
  | `Array_pat_n x -> PatList (array_pattern_n env x |> fb)
  | `Find_pat_body x -> PatList (find_pattern_body env x |> fb)
  | `Hash_pat_body x -> PatList (hash_pattern_body env x |> fb)

and expression (env : env) (x : CST.expression) : AST.expr =
  match x with
  | `Cmd_bin (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        match v2 with
        | `Or tok -> (Op_kOR, token2 env tok)
        | `And tok -> (Op_kAND, token2 env tok)
      in
      let v3 = expression env v3 in
      Binop (v1, v2, v3)
  | `Cmd_un x -> command_unary env x
  | `Cmd_assign x -> command_assignment env x
  | `Cmd_op_assign (v1, v2, v3) ->
      let v1 = lhs env v1 in
      let op, tok =
        match v2 with
        | `PLUSEQ tok -> (B Op_PLUS, token2 env tok)
        | `DASHEQ tok -> (B Op_MINUS, token2 env tok)
        | `STAREQ tok -> (B Op_TIMES, token2 env tok)
        | `STARSTAREQ tok -> (B Op_POW, token2 env tok)
        | `SLASHEQ tok -> (B Op_DIV, token2 env tok)
        | `BARBAREQ tok -> (Op_OR, token2 env tok)
        | `BAREQ tok -> (B Op_BOR, token2 env tok)
        | `AMPAMPEQ tok -> (Op_AND, token2 env tok)
        | `AMPEQ tok -> (B Op_BAND, token2 env tok)
        | `PERCEQ tok -> (B Op_REM, token2 env tok)
        | `GTGTEQ tok -> (B Op_RSHIFT, token2 env tok)
        | `LTLTEQ tok -> (B Op_LSHIFT, token2 env tok)
        | `HATEQ tok -> (B Op_XOR, token2 env tok)
      in
      let v3 =
        match v3 with
        | `Exp x -> expression env x
        | `Rescue_modi_exp (v1, v2, v3) ->
            let v1 = expression env v1 in
            let v2 = (* "rescue" *) token2 env v2 in
            let v3 = arg env v3 in
            Rescue (v1, v2, v3)
      in
      Binop (v1, (Op_OP_ASGN op, tok), v3)
  | `Cmd_call (v1, v2) ->
      let v1 =
        match v1 with
        | `Call_ x -> call_ env x
        | `Chai_cmd_call x -> chained_command_call env x
        | `Choice_var x -> (
            match x with
            | `Var x -> variable env x
            | `Func_id x -> Id (function_identifier env x, ID_Lowercase))
      in
      let v2 = command_argument_list env v2 in
      Call (v1, fb v2, None)
  | `Cmd_call_with_blk x -> command_call_with_block env x
  | `Chai_cmd_call x -> chained_command_call env x
  | `Ret_cmd (v1, v2) ->
      let v1 = token2 env v1 in
      let v2 = command_argument_list env v2 in
      S (Return (v1, v2))
  | `Yield_cmd (v1, v2) ->
      let v1 = token2 env v1 in
      let v2 = command_argument_list env v2 in
      S (Yield (v1, v2))
  | `Brk_cmd (v1, v2) ->
      let v1 = token2 env v1 in
      let v2 = command_argument_list env v2 in
      S (Break (v1, v2))
  | `Next_cmd (v1, v2) ->
      let v1 = token2 env v1 in
      let v2 = command_argument_list env v2 in
      S (Next (v1, v2))
  | `Match_pat (v1, v2, v3) ->
      (* https://womanonrails.com/ruby-pattern-matching-second-look
         This one acts similarly to the below, but might produce a
         binding or raise an exception.
         Otherwise, it still returns a boolean. So I don't really
         care, and will translate them the same.
      *)
      (* coupling: match pattern
         Notably, this looks identical to a mapping pair, which might
         look something like
         foo ..., :key => val, ...
         This means a user might write a pattern like
         $KEY => $VAL
         and really mean that, and not this match pattern thing.
         For consistency with that, we will prefer to parse it as a hash
         pair, in the case where we are parsing a pattern.
         See the other "coupling: match pattern" for where we handle this.
      *)
      let v1 = arg env v1 in
      let v2 = (* "=>" *) token2 env v2 in
      let v3 = pattern_top_expr_body env v3 in
      Match (v1, v2, v3)
  | `Test_pat (v1, v2, v3) ->
      (* https://womanonrails.com/ruby-pattern-matching-second-look
         This one must return a boolean on whether the expr matches
         the pattern or not.
      *)
      let v1 = arg env v1 in
      let v2 = (* "in" *) token2 env v2 in
      let v3 = pattern_top_expr_body env v3 in
      Match (v1, v2, v3)
  | `Arg x -> arg env x

and pow (env : env) ((v1, v2, v3) : CST.pow) : expr =
  let v1 = simple_numeric env v1 in
  let v2 = (* binary_star_star *) token2 env v2 in
  let v3 = arg env v3 in
  Binop (Literal v1, (B Op_POW, v2), v3)

and arg (env : env) (x : CST.arg) : AST.expr =
  match x with
  | `Un_minus_pow (v1, v2) ->
      let v1 = (* unary_minus_num *) token2 env v1 in
      let v2 = pow env v2 in
      Unary ((U Op_UMinus, v1), v2)
  | `Prim x -> primary env x
  | `Assign x -> assignment env x
  | `Op_assign (v1, v2, v3) ->
      let v1 = lhs env v1 in
      let op, tok =
        match v2 with
        | `PLUSEQ tok -> (B Op_PLUS, token2 env tok)
        | `DASHEQ tok -> (B Op_MINUS, token2 env tok)
        | `STAREQ tok -> (B Op_TIMES, token2 env tok)
        | `STARSTAREQ tok -> (B Op_POW, token2 env tok)
        | `SLASHEQ tok -> (B Op_DIV, token2 env tok)
        | `BARBAREQ tok -> (Op_OR, token2 env tok)
        | `BAREQ tok -> (B Op_BOR, token2 env tok)
        | `AMPAMPEQ tok -> (Op_AND, token2 env tok)
        | `AMPEQ tok -> (B Op_BAND, token2 env tok)
        | `PERCEQ tok -> (B Op_REM, token2 env tok)
        | `GTGTEQ tok -> (B Op_RSHIFT, token2 env tok)
        | `LTLTEQ tok -> (B Op_LSHIFT, token2 env tok)
        | `HATEQ tok -> (B Op_XOR, token2 env tok)
      in
      let v3 = arg_rhs env v3 in
      Binop (v1, (Op_OP_ASGN op, tok), v3)
  | `Cond (v1, v2, v3, v4, v5) ->
      let v1 = arg env v1 in
      let v2 = token2 env v2 in
      let v3 = arg env v3 in
      let v4 = token2 env v4 in
      let v5 = arg env v5 in
      Ternary (v1, v2, v3, v4, v5)
  | `Range x -> range env x
  | `Bin x -> binary env x
  | `Un x -> unary env x

and arg_rhs (env : env) (x : CST.arg_rhs) =
  match x with
  | `Arg x -> arg env x
  | `Rescue_modi_arg (v1, v2, v3) ->
      let v1 = arg env v1 in
      let v2 = (* "rescue" *) token2 env v2 in
      let v3 = arg env v3 in
      Rescue (v1, v2, v3)

and anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e (env : env)
    ((v1, v2) : CST.anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e) =
  let v1 = literal_contents env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token2 env v1 (* pattern \s+ *) in
        let v2 = literal_contents env v2 in
        v2)
      v2
  in
  v1 :: v2

and lambda (env : env) ((v1, v2, v3) : CST.lambda) =
  let v1 = token2 env v1 in
  let v2 =
    match v2 with
    | Some x ->
        Some
          (match x with
          | `Params x -> parameters env x |> Tok.unbracket
          | `Bare_params x -> bare_parameters env x)
    | None -> None
  in
  let v3 =
    match v3 with
    | `Blk x -> block env x
    | `Do_blk x -> do_block env x
  in
  let b = false in
  (* should have a third option for lambdas *)
  CodeBlock ((v1, b, v1), v2, [ v3 ])

and hash (env : env) (v1, v2, v3) =
  let v1 = token2 env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 =
          match v1 with
          | `Pair x -> pair_for_hash env x
          | `Hash_splat_arg x -> hash_splat_argument env x
        in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = token2 env v1 in
              let v2 =
                match v2 with
                | `Pair x -> pair_for_hash env x
                | `Hash_splat_arg x -> hash_splat_argument env x
              in
              v2)
            v2
        in
        let _v3 =
          match v3 with
          | Some _tok -> ()
          | None -> ()
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token2 env v3 in
  (v1, v2, v3)

and primary (env : env) (x : CST.primary) : AST.expr =
  match x with
  | `Semg_ellips tok
  | `Semg_ellips_foll_by_nl tok -> (
      match env.extra with
      | Program ->
          (* This is an example of argument forwarding.
             Let's just consider it an identifier named ...
          *)
          Id (str env tok, ID_Lowercase)
      | Pattern -> Ellipsis (token2 env tok))
  | `Deep_ellips (l, e, r) ->
      if_in_pattern env
        (DeepEllipsis (token2 env l, expression env e, token2 env r))
  | `Choice_paren_stmts x -> (
      match x with
      | `Paren_stmts x ->
          let lp, xs, rp = parenthesized_statements env x in
          S (Block (lp, xs, rp))
      | `Lhs x -> lhs env x
      | `Func_id_call x -> Id (function_identifier env x, ID_Lowercase)
      | `Call x -> call env x
      | `Array (v1, v2, v3) ->
          let lb = token2 env v1 in
          let v2 =
            match v2 with
            | Some x -> argument_list_with_trailing_comma env x
            | None -> []
          in
          let rb = token2 env v3 in
          Array (lb, v2, rb)
      (* ?? *)
      | `Str_array (v1, v2, v3, v4, v5) -> string_array env (v1, v2, v3, v4, v5)
      | `Symb_array (v1, v2, v3, v4, v5) -> symbol_array env (v1, v2, v3, v4, v5)
      | `Hash x ->
          let v1, v2, v3 = hash env x in
          Hash (true, (v1, v2, v3))
      | `Subs (v1, v2, v3) -> subshell env (v1, v2, v3)
      | `Lit x -> literal env x
      | `Str x -> Literal (String (mk_string_kind (string_ env x)))
      | `Char tok -> Literal (Char (str env tok))
      (* ??? *)
      | `Chai_str (v1, v2) ->
          let l, v1, r = string_ env v1 in
          let v2 =
            List.concat_map
              (fun x ->
                let _lp, x, _ = string_ env x in
                x)
              v2
          in
          Literal (String (Double (l, v1 @ v2, r)))
      | `Regex (v1, v2, v3) ->
          let r = regex env (v1, v2, v3) in
          (* TODO: no modifier in Ruby grammar.js? *)
          Literal (Regexp (r, None))
      | `Lambda (v1, v2, v3) -> lambda env (v1, v2, v3)
      | `Meth (v1, v2) ->
          let v1 = token2 env v1 in
          let n, params, body_exn = method_rest env v2 in
          D (MethodDef (v1, M n, params, body_exn))
      | `Sing_meth (v1, v2, v3, v4) ->
          let v1 = token2 env v1 in
          let v2 =
            match v2 with
            | `Var x -> variable env x
            | `LPAR_arg_RPAR (v1, v2, v3) ->
                let _lp = token2 env v1 in
                let v2 = arg env v2 in
                let _rp = token2 env v3 in
                v2
          in
          let v3 =
            match v3 with
            | `DOT tok -> fun a b -> DotAccess (a, token2 env tok, b)
            | `COLONCOLON tok ->
                fun a b -> ScopedId (Scope (a, token2 env tok, SM b))
          in
          let n, params, body_exn = method_rest env v4 in
          let n = v3 v2 n in
          D (MethodDef (v1, SingletonM n, params, body_exn))
      | `Class (v1, v2, v3, v4, v5) ->
          let v1 = token2 env v1 in
          let v2 =
            match v2 with
            | `Cst x ->
                let id, _ = constant env x in
                NameConstant id
            | `Scope_resol x -> NameScope (scope_resolution env x)
          in
          let v3 =
            match v3 with
            | `Supe_term (v1, v2) ->
                let v1 = superclass env v1 in
                let _v2 = terminator env v2 in
                Some v1
            | `Opt_term _ -> None
          in
          let v4 =
            match v4 with
            | None -> empty_body_exn
            | Some v4 -> body_statement env v4
          in
          let _v5 = (* "end" *) token2 env v5 in
          D (ClassDef (v1, C (v2, v3), v4))
      | `Sing_class (v1, v2, v3, v4, v5, v6) ->
          let v1 = (* class *) token2 env v1 in
          let v2 = (* singleton_class_left_angle_left_langle *) token2 env v2 in
          let v3 = arg env v3 in
          let _v4 = terminator env v4 in
          let v5 =
            match v5 with
            | None -> empty_body_exn
            | Some v5 -> body_statement env v5
          in
          let _v6 = (* "end" *) token2 env v6 in
          D (ClassDef (v1, SingletonC (v2, v3), v5))
      | `Module (v1, v2, v3, v4, v5) ->
          let v1 = (* module *) token2 env v1 in
          let v2 =
            match v2 with
            | `Cst x ->
                let id, _ = constant env x in
                NameConstant id
            | `Scope_resol x -> NameScope (scope_resolution env x)
          in
          let _v3 =
            match v3 with
            | None -> None
            | Some x -> Some (terminator env x)
          in
          let v4 =
            match v4 with
            | None -> empty_body_exn
            | Some x -> body_statement env x
          in
          let _v5 = (* end *) token2 env v5 in
          D (ModuleDef (v1, v2, v4))
      | `Begin (v1, v2, v3, v4) ->
          let tbegin = (* "begin" *) token2 env v1 in
          let _v2 =
            match v2 with
            | Some x -> terminator env x
            | None -> ()
          in
          let v3 =
            match v3 with
            | None -> empty_body_exn
            | Some v3 -> body_statement env v3
          in
          let v4 = (* "end" *) token2 env v4 in
          S (Block (tbegin, [ S (ExnBlock v3) ], v4))
      | `While (v1, v2, v3) ->
          let v1 = token2 env v1 (* "while" *) in
          let v2 = statement env v2 (* condition *) in
          let v3 = do_ env v3 (* body *) in
          S (While (v1, true, v2, v3))
      | `Until (v1, v2, v3) ->
          let v1 = token2 env v1 (* "until" *) in
          let v2 = statement env v2 (* condition *) in
          let v3 = do_ env v3 (* body *) in
          S (Until (v1, true, v2, v3))
      | `If (v1, v2, v3, v4, v5) ->
          let v1 = token2 env v1 in
          let v2 = statement env v2 in
          let v3 =
            match v3 with
            | `Term x ->
                let _ = terminator env x in
                []
            | `Then x -> then_ env x
          in
          let v4 =
            match v4 with
            | Some x ->
                Some
                  (match x with
                  | `Else x -> else_ env x
                  | `Elsif x ->
                      let t, s = elsif env x in
                      (t, [ S s ]))
            | None -> None
          in
          let _v5 = token2 env v5 in
          S (If (v1, v2, v3, v4))
      | `Unless (v1, v2, v3, v4, v5) ->
          let v1 = token2 env v1 in
          let v2 = statement env v2 in
          let v3 =
            match v3 with
            | `Term x ->
                let _ = terminator env x in
                []
            | `Then x -> then_ env x
          in
          let v4 =
            match v4 with
            | Some x ->
                Some
                  (match x with
                  | `Else x -> else_ env x
                  | `Elsif x ->
                      let t, s = elsif env x in
                      (t, [ S s ]))
            | None -> None
          in
          let _v5 = token2 env v5 in
          S (Unless (v1, v2, v3, v4))
      | `For (v1, v2, v3, v4) ->
          let v1 = token2 env v1 (* "for" *) in
          let v2 =
            match v2 with
            | `Lhs x -> lhs env x
            | `Left_assign_list x -> left_assignment_list env x
          in
          let t, e = in_ env v3 in
          let v4 = do_ env v4 in
          S (For (v1, PatExpr v2, t, e, v4))
      | `Case (v1, v2, v3, v5, v6, v7) ->
          let v1 = token2 env v1 in
          let v2 =
            match v2 with
            | Some x -> Some (statement env x)
            | None -> None
          in
          let _v3 =
            match v3 with
            | Some x -> Some (terminator env x)
            | None -> None
          in
          let v5 = List_.map (when_ env) v5 in
          let v6 =
            match v6 with
            | Some x -> Some (else_ env x)
            | None -> None
          in
          let _v7 = token2 env v7 in
          S (Case (v1, { case_guard = v2; case_whens = v5; case_else = v6 }))
      | `Case_match (v1, v2, v3, v4, v5, v6) ->
          let v1 = (* "case" *) token2 env v1 in
          let v2 = statement env v2 in
          let _v3 =
            match v3 with
            | Some x -> terminator env x
            | None -> ()
          in
          let v4 = List_.map (in_clause env) v4 in
          let v5 =
            match v5 with
            | Some x -> Some (else_ env x)
            | None -> None
          in
          let _v6 = (* "end" *) token2 env v6 in
          S
            (Case (v1, { case_guard = Some v2; case_whens = v4; case_else = v5 }))
      | `Ret (v1, v2) ->
          let v1 = token2 env v1 in
          let v2 =
            match v2 with
            | Some x -> argument_list env x |> Tok.unbracket
            | None -> []
          in
          S (Return (v1, v2))
      | `Yield (v1, v2) ->
          let v1 = token2 env v1 in
          let v2 =
            match v2 with
            | Some x -> argument_list env x |> Tok.unbracket
            | None -> []
          in
          S (Yield (v1, v2))
      | `Brk (v1, v2) ->
          let v1 = token2 env v1 in
          let v2 =
            match v2 with
            | Some x -> argument_list env x |> Tok.unbracket
            | None -> []
          in
          S (Break (v1, v2))
      | `Next (v1, v2) ->
          let v1 = token2 env v1 in
          let v2 =
            match v2 with
            | Some x -> argument_list env x |> Tok.unbracket
            | None -> []
          in
          S (Next (v1, v2))
      | `Redo (v1, v2) ->
          let v1 = token2 env v1 in
          let v2 =
            match v2 with
            | Some x -> argument_list env x |> Tok.unbracket
            | None -> []
          in
          S (Redo (v1, v2))
      | `Retry (v1, v2) ->
          let v1 = token2 env v1 in
          let v2 =
            match v2 with
            | Some x -> argument_list env x |> Tok.unbracket
            | None -> []
          in
          S (Retry (v1, v2))
      | `Paren_un (v1, v2) ->
          let v1 =
            match v1 with
            | `Defi tok -> (Op_DefinedQuestion, token2 env tok)
            | `Not tok -> (Op_UNot, token2 env tok)
          in
          let lp, v2, rp = parenthesized_statements env v2 in
          let block = S (Block (lp, v2, rp)) in
          Unary (v1, block)
      | `Here_begin tok ->
          let x = str env tok in
          Literal (String (Single x)))

and guard (env : env) (x : CST.guard) =
  match x with
  | `If_guard (v1, v2) ->
      let _v1 = (* "if" *) token2 env v1 in
      let v2 = expression env v2 in
      v2
  | `Unless_guard (v1, v2) ->
      let v1 = (* "unless" *) token2 env v1 in
      let v2 = expression env v2 in
      Unary ((Op_UNot, v1), v2)

and in_clause (env : env) ((v1, v2, v3, v4) : CST.in_clause) =
  let v1 = (* "in" *) token2 env v1 in
  let v2 = pattern_top_expr_body env v2 in
  let v3 =
    match v3 with
    | Some x ->
        let g = guard env x in
        PatWhen (v2, g)
    | None -> v2
  in
  let v4 =
    match v4 with
    | `Term _x -> []
    | `Then x -> then_ env x
  in
  (v1, [ v3 ], v4)

and parenthesized_statements (env : env)
    ((v1, v2, v3) : CST.parenthesized_statements) =
  let v1 = token2 env v1 in
  let v2 =
    match v2 with
    | Some x -> statements env x
    | None -> []
  in
  let v3 = token2 env v3 in
  (v1, v2, v3)

and scope_resolution (env : env) ((v1, v2) : CST.scope_resolution) :
    AST.scope_resolution =
  let v1 =
    match v1 with
    | `COLONCOLON tok -> fun e -> TopScope (token2 env tok, e)
    | `Prim_imm_tok_colo (v1, v2) ->
        let v1 = primary env v1 in
        let v2 = token2 env v2 in
        fun e -> Scope (v1, v2, SV e)
  in
  let v2 = (* constant *) constant env v2 in
  v1 v2

and anon_choice_for_call_no_id (env : env) x =
  match x with
  | `Id tok -> (
      let s, t = str env tok in
      match s with
      | "..." ->
          (* This must mean we are meaning to have a DotAccessEllipsis. *)
          Left t
      | _ -> Right (MethodId (str env tok, ID_Lowercase)))
  | `Op x -> (
      let op = operator env x in
      match op with
      | Left bin, t -> Right (MethodOperator (bin, t))
      | Right un, t -> Right (MethodUOperator (un, t)))
  | `Cst x -> Right (MethodId (constant env x))
  | `Func_id x -> Right (MethodId (function_identifier env x, ID_Lowercase))

and call_ (env : env) ((v1, v2, v3) : CST.call_) =
  let v1 = primary env v1 in
  let v2 = call_operator env v2 in
  let v3 = anon_choice_for_call_no_id env v3 in
  match v3 with
  | Left tok -> DotAccessEllipsis (v1, tok)
  | Right v3 -> DotAccess (v1, v2, v3)

and chained_command_call (env : env) ((v1, v2, v3) : CST.chained_command_call) :
    AST.expr =
  let v1 = command_call_with_block env v1 in
  let v2 = call_operator env v2 in
  let v3 = anon_choice_for_call_no_id env v3 in
  match v3 with
  | Left tok -> DotAccessEllipsis (v1, tok)
  | Right v3 -> DotAccess (v1, v2, v3)

and command_call_with_block (env : env) (x : CST.command_call_with_block) :
    AST.expr =
  match x with
  | `Choice_choice_call__cmd_arg_list_blk x -> (
      match x with
      | `Choice_call__cmd_arg_list_blk (v1, v2, v3) ->
          let v1 = anon_choice_call__23b9492 env v1 in
          let v2 = command_argument_list env v2 in
          let v3 = block env v3 in
          Call (v1, fb v2, Some v3)
      | `Choice_call__cmd_arg_list_do_blk (v1, v2, v3) ->
          let v1 = anon_choice_call__23b9492 env v1 in
          let v2 = command_argument_list env v2 in
          let v3 = do_block env v3 in
          Call (v1, fb v2, Some v3))
  | `Arg_DOTDOTDOT_do_blk (v1, v2, v3) -> (
      let v1 = arg env v1 in
      let v2 = (* "..." *) token2 env v2 in
      let v3 = do_block env v3 in
      match env.extra with
      | Pattern -> Call (v1, fb [ Arg (Ellipsis v2) ], Some v3)
      | Program ->
          (* This shouldn't actually happen in a non-pattern case. *)
          failwith "invalid program")
  | `Arg_DOTDOTDOT_blk (v1, v2, v3) -> (
      let v1 = arg env v1 in
      let v2 = (* "..." *) token2 env v2 in
      let v3 = block env v3 in
      match env.extra with
      | Pattern -> Call (v1, fb [ Arg (Ellipsis v2) ], Some v3)
      | Program ->
          (* This shouldn't actually happen in a non-pattern case. *)
          failwith "invalid program")

and anon_choice_var_2a392d7 (env : env) (x : CST.anon_choice_var_2a392d7) : expr
    =
  match x with
  | `Var x -> variable env x
  | `Func_id x -> Id (function_identifier env x, ID_Lowercase)

and anon_choice_call__23b9492 (env : env) (x : CST.anon_choice_call__23b9492) :
    expr =
  match x with
  | `Call_ x -> call_ env x
  | `Choice_var x -> anon_choice_var_2a392d7 env x

and anon_choice_choice_call__cfb94af (env : env)
    (x : CST.anon_choice_choice_call__cfb94af) =
  match x with
  | `Choice_call_ x -> anon_choice_call__23b9492 env x
  | `Prim_choice_DOT (v1, v2) ->
      let v1 = primary env v1 in
      let _v2 = call_operator env v2 in
      v1

and call (env : env) (x : CST.call) : AST.expr =
  match x with
  | `Choice_choice_call__arg_list (v1, v2) ->
      let v1 = anon_choice_choice_call__cfb94af env v1 in
      let v2 = argument_list env v2 in
      Call (v1, v2, None)
  | `Choice_choice_call__arg_list_blk (v1, v2, v3) ->
      let v1 = anon_choice_choice_call__cfb94af env v1 in
      let v2 = argument_list env v2 in
      let v3 = block env v3 in
      Call (v1, v2, Some v3)
  | `Choice_choice_call__arg_list_do_blk (v1, v2, v3) ->
      let v1 = anon_choice_choice_call__cfb94af env v1 in
      let v2 = argument_list env v2 in
      let v3 = do_block env v3 in
      Call (v1, v2, Some v3)
  | `Choice_call__blk (v1, v2) ->
      let v1 = anon_choice_call__23b9492 env v1 in
      let v2 = block env v2 in
      Call (v1, fb [], Some v2)
  | `Choice_call__do_blk (v1, v2) ->
      let v1 = anon_choice_call__23b9492 env v1 in
      let v2 = do_block env v2 in
      Call (v1, fb [], Some v2)

and command_argument_list (env : env) (x : CST.command_argument_list) :
    AST.argument list =
  match x with
  (* See the accompanying `grammar.js`.
   * This is a weird case. This is essentially where we have fused the
   * ellipsis together with the comma, so that we can get an extra
   * token of lookahead.
   * To properly report the range for the ellipsis, we have to separate
   * it from the comma.
   *)
  | `Tok_prec_p1000_dotd_comma_arg_rep_COMMA_arg (v1, v2, v3) ->
      let t = token2 env v1 in
      (* The start position should be correct, so all we have to do
         is change the contents of the token to be just the ellipsis.
      *)
      let t = Tok.rewrap_str "..." t in
      let v2 = argument env v2 in
      let v3 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token2 env v1 in
            let v2 = argument env v2 in
            v2)
          v3
      in
      Arg (Ellipsis t) :: v2 :: v3
  | `Arg_rep_COMMA_arg (v1, v2) ->
      let v1 = argument env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let _t = token2 env v1 in
            let v2 = argument env v2 in
            v2)
          v2
      in
      v1 :: v2

and argument_list (env : env) ((v1, v2, v3) : CST.argument_list) :
    AST.argument list AST.bracket =
  let lp = token2 env v1 in
  let v2 =
    match v2 with
    | Some x -> argument_list_with_trailing_comma env x
    | None -> []
  in
  let rp = token2 env v3 in
  (lp, v2, rp)

and argument_list_with_trailing_comma (env : env)
    ((v1, v2, v3) : CST.argument_list_with_trailing_comma) : AST.argument list =
  let v1 = argument env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _ = token2 env v1 in
        let v2 = argument env v2 in
        v2)
      v2
  in
  let _v3 =
    match v3 with
    | Some _tok -> ()
    | None -> ()
  in
  v1 :: v2

and argument (env : env) (x : CST.argument) : AST.argument =
  match x with
  | `Forw_arg tok -> (
      let t = (* "..." *) token2 env tok in
      match env.extra with
      | Program ->
          (* Close enough. It's not actually strictly a splat, but it behaves
             similarly.
          *)
          Arg (Splat (t, None))
      | Pattern -> Arg (Ellipsis t))
  | `Exp x -> Arg (expression env x)
  | `Splat_arg x -> Arg (splat_argument env x)
  | `Hash_splat_arg x -> Arg (hash_splat_argument env x)
  | `Blk_arg (v1, v2) -> (
      let v1 = token2 env v1 in
      match v2 with
      | None -> ArgAmp v1
      | Some v2 ->
          let v2 = arg env v2 in
          Arg (Unary ((Op_UAmper, v1), v2)))
  | `Pair x -> pair_for_argument env x

and splat_argument (env : env) ((v1, v2) : CST.splat_argument) =
  let v1 = token2 env v1 in
  match v2 with
  | None -> Splat (v1, None)
  | Some v2 ->
      let v2 = arg env v2 in
      Splat (v1, Some v2)

and hash_splat_argument (env : env) ((v1, v2) : CST.hash_splat_argument) =
  let v1 = token2 env v1 (* hash_splat_star_star *) in
  match v2 with
  | None -> TodoExpr ("HashSplatArg", v1)
  | Some v2 ->
      let v2 = arg env v2 in
      Unary ((Op_UStarStar, v1), v2)

and do_ (env : env) ((v1, v2, v3) : CST.do_) : AST.expr list =
  (match v1 with
  | `Do _tok (* "do" *) -> ()
  | `Term x -> terminator env x);
  let v2 =
    match v2 with
    | Some x -> statements env x
    | None -> []
  in
  let _v3 = token2 env v3 (* "end" *) in
  v2

and do_block (env : env) ((v1, v2, v3, v4, v5) : CST.do_block) : AST.expr =
  let tdo = (* "do" *) token2 env v1 in
  let _v2 =
    match v2 with
    | Some x -> terminator env x
    | None -> ()
  in
  let params_opt =
    match v3 with
    | Some (v1, v2) ->
        let v1 = block_parameters env v1 |> Tok.unbracket in
        let _v2 =
          match v2 with
          | Some x -> terminator env x
          | None -> ()
        in
        Some v1
    | None -> None
  in
  (* We do this _to_exprs thing instead of calling just body_statements because
     we don't want to wrap the contents of this block in yet another block.
     This will cause us to get Block(Block ...) after Generic translation.
  *)
  let exprs =
    match v4 with
    | None -> []
    | Some v4 -> body_statement_to_exprs env v4
  in
  let tend = (* "tend" *) token2 env v5 in
  CodeBlock ((tdo, false, tend), params_opt, exprs)

and block (env : env) ((v1, v2, v3, v4) : CST.block) =
  let lb = token2 env v1 in
  let params_opt =
    match v2 with
    | Some x -> Some (block_parameters env x |> Tok.unbracket)
    | None -> None
  in
  let v3 =
    match v3 with
    | Some x -> statements env x
    | None -> []
  in
  let rb = token2 env v4 in
  CodeBlock ((lb, true, rb), params_opt, v3)

and assignment (env : env) (x : CST.assignment) =
  match x with
  | `Choice_lhs_EQ_choice_choice_arg (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Lhs x -> lhs env x
        | `Left_assign_list x -> left_assignment_list env x
      in
      let v2 = token2 env v2 in
      let v3 =
        match v3 with
        | `Choice_arg x -> arg_rhs env x
        | `Splat_arg x -> splat_argument env x
        | `Right_assign_list x ->
            right_assignment_list env x |> list_to_maybe_tuple
      in
      Binop (v1, (Op_ASSIGN, v2), v3)

and command_assignment (env : env) ((v1, v2, v3) : CST.command_assignment) =
  let v1 =
    match v1 with
    | `Lhs x -> lhs env x
    | `Left_assign_list x -> left_assignment_list env x
  in
  let v2 = token2 env v2 in
  let v3 =
    match v3 with
    | `Exp x -> expression env x
    | `Rescue_modi_exp (v1, v2, v3) ->
        let v1 = expression env v1 in
        let v2 = (* "rescue" *) token2 env v2 in
        let v3 = arg env v3 in
        Rescue (v1, v2, v3)
  in
  Binop (v1, (Op_ASSIGN, v2), v3)

and binary (env : env) (x : CST.binary) =
  match x with
  | `Arg_and_arg (v1, v2, v3) ->
      let v1 = arg env v1 in
      let v2 = token2 env v2 in
      let v3 = arg env v3 in
      Binop (v1, (Op_kAND, v2), v3)
  | `Arg_or_arg (v1, v2, v3) ->
      let v1 = arg env v1 in
      let v2 = token2 env v2 in
      let v3 = arg env v3 in
      Binop (v1, (Op_kOR, v2), v3)
  | `Arg_BARBAR_arg (v1, v2, v3) ->
      let v1 = arg env v1 in
      let v2 = token2 env v2 in
      let v3 = arg env v3 in
      Binop (v1, (Op_OR, v2), v3)
  | `Arg_AMPAMP_arg (v1, v2, v3) ->
      let v1 = arg env v1 in
      let v2 = token2 env v2 in
      let v3 = arg env v3 in
      Binop (v1, (Op_AND, v2), v3)
  | `Arg_choice_LTLT_arg (v1, v2, v3) ->
      let v1 = arg env v1 in
      let v2 =
        match v2 with
        | `LTLT tok -> (B Op_LSHIFT, token2 env tok)
        | `GTGT tok -> (B Op_RSHIFT, token2 env tok)
      in
      let v3 = arg env v3 in
      Binop (v1, v2, v3)
  | `Arg_choice_LT_arg (v1, v2, v3) ->
      let v1 = arg env v1 in
      let v2 =
        match v2 with
        | `LT tok -> (B Op_LT, token2 env tok)
        | `LTEQ tok -> (B Op_LEQ, token2 env tok)
        | `GT tok -> (B Op_GT, token2 env tok)
        | `GTEQ tok -> (B Op_GEQ, token2 env tok)
      in
      let v3 = arg env v3 in
      Binop (v1, v2, v3)
  | `Arg_AMP_arg (v1, v2, v3) ->
      let v1 = arg env v1 in
      let v2 = token2 env v2 in
      let v3 = arg env v3 in
      Binop (v1, (B Op_BAND, v2), v3)
  | `Arg_choice_HAT_arg (v1, v2, v3) ->
      let v1 = arg env v1 in
      let v2 =
        match v2 with
        | `HAT tok -> (B Op_XOR, token2 env tok)
        | `BAR tok -> (B Op_BOR, token2 env tok)
      in
      let v3 = arg env v3 in
      Binop (v1, v2, v3)
  | `Arg_choice_PLUS_arg (v1, v2, v3) ->
      let v1 = arg env v1 in
      let v2 =
        match v2 with
        | `PLUS tok -> (B Op_PLUS, token2 env tok)
        | `Bin_minus tok -> (B Op_MINUS, token2 env tok)
      in
      let v3 = arg env v3 in
      Binop (v1, v2, v3)
  | `Arg_choice_SLASH_arg (v1, v2, v3) ->
      let v1 = arg env v1 in
      let v2 =
        match v2 with
        | `SLASH tok -> (B Op_DIV, token2 env tok)
        | `PERC tok -> (B Op_REM, token2 env tok)
        | `Bin_star tok -> (B Op_TIMES, token2 env tok)
      in
      let v3 = arg env v3 in
      Binop (v1, v2, v3)
  | `Arg_choice_EQEQ_arg (v1, v2, v3) ->
      let v1 = arg env v1 in
      let v2 =
        match v2 with
        | `EQEQ tok -> (B Op_EQ, token2 env tok)
        | `BANGEQ tok -> (B Op_NEQ, token2 env tok)
        | `EQEQEQ tok -> (B Op_EQQ, token2 env tok)
        | `LTEQGT tok -> (B Op_CMP, token2 env tok)
        | `EQTILDE tok -> (B Op_MATCH, token2 env tok)
        | `BANGTILDE tok -> (B Op_NMATCH, token2 env tok)
      in
      let v3 = arg env v3 in
      Binop (v1, v2, v3)
  | `Arg_bin_star_star_arg (v1, v2, v3) ->
      let v1 = arg env v1 in
      let v2 = token2 env v2 in
      let v3 = arg env v3 in
      Binop (v1, (Op_kAND, v2), v3)

and unary (env : env) (x : CST.unary) : AST.expr =
  match x with
  | `Defi_arg (v1, v2) ->
      let v1 = token2 env v1 in
      let v2 = arg env v2 in
      Unary ((Op_DefinedQuestion, v1), v2)
  | `Not_arg (v1, v2) ->
      let v1 = token2 env v1 in
      let v2 = arg env v2 in
      Unary ((Op_UNot, v1), v2)
  | `Choice_un_minus_arg (v1, v2) ->
      let v1 =
        match v1 with
        | `Un_minus tok -> (U Op_UMinus, token2 env tok)
        | `PLUS tok -> (U Op_UPlus, token2 env tok)
        | `Bin_minus tok ->
            (* I don't know why this is here, but I assume it's because the
               grammar isn't perfect and sometimes allows a binary minus token
               through.
            *)
            (U Op_UMinus, token2 env tok)
      in
      let v2 = arg env v2 in
      Unary (v1, v2)
  | `Choice_BANG_arg (v1, v2) ->
      let v1 =
        match v1 with
        | `BANG tok -> (U Op_UBang, token2 env tok)
        | `TILDE tok -> (U Op_UTilde, token2 env tok)
      in
      let v2 = arg env v2 in
      Unary (v1, v2)

and command_unary (env : env) (x : CST.command_unary) : AST.expr =
  match x with
  | `Defi_exp (v1, v2) ->
      let v1 = token2 env v1 in
      let v2 = expression env v2 in
      Unary ((Op_DefinedQuestion, v1), v2)
  | `Not_exp (v1, v2) ->
      let v1 = token2 env v1 in
      let v2 = expression env v2 in
      Unary ((Op_UNot, v1), v2)
  | `Choice_un_minus_exp (v1, v2) ->
      let v1 =
        match v1 with
        | `Un_minus tok -> (U Op_UMinus, token2 env tok)
        | `PLUS tok -> (U Op_UPlus, token2 env tok)
      in
      let v2 = expression env v2 in
      Unary (v1, v2)
  | `Choice_BANG_exp (v1, v2) ->
      let v1 =
        match v1 with
        | `BANG tok -> (U Op_UBang, token2 env tok)
        | `TILDE tok -> (U Op_UTilde, token2 env tok)
      in
      let v2 = expression env v2 in
      Unary (v1, v2)

and anon_choice_DOTDOT_ed078ec (env : env) (x : CST.anon_choice_DOTDOT_ed078ec)
    =
  match x with
  | `DOTDOT tok -> (B Op_DOT2, token2 env tok)
  | `DOTDOTDOT tok -> (Op_DOT3, token2 env tok)

and range (env : env) (x : CST.range) : AST.expr =
  (* A note: if we are parsing a pattern, someone writes a
     command call expression with arbitrary arguments, they might write

     foo ...

     which looks exactly like a range expression.
     So when parsing a pattern, let's prefer the command call version,
     which is more likely to come up.
  *)
  match x with
  | `Arg_choice_DOTDOT_arg (v1, v2, v3) ->
      let v1 = arg env v1 in
      let v2 = anon_choice_DOTDOT_ed078ec env v2 in
      Binop (v1, v2, arg env v3)
  | `Choice_DOTDOT_arg (v1, v2) ->
      let v1 = anon_choice_DOTDOT_ed078ec env v1 in
      let v2 = arg env v2 in
      let t = snd v1 in
      Binop (fake_nil t, v1, v2)
  | `Arg_choice_DOTDOT (v1, v2) -> (
      let v1 = arg env v1 in
      let ((_, v2_tok) as v2) = anon_choice_DOTDOT_ed078ec env v2 in
      let t = snd v2 in
      match env.extra with
      | Pattern -> Call (v1, fb [ Arg (Ellipsis v2_tok) ], None)
      | _ -> Binop (v1, v2, fake_nil t))

and right_assignment_list (env : env) ((v1, v2) : CST.right_assignment_list) :
    AST.expr list =
  let v1 =
    match v1 with
    | `Arg x -> arg env x
    | `Splat_arg x -> splat_argument env x
  in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token2 env v1 in
        let v2 =
          match v2 with
          | `Arg x -> arg env x
          | `Splat_arg x -> splat_argument env x
        in
        v2)
      v2
  in
  v1 :: v2

and left_assignment_list (env : env) (x : CST.left_assignment_list) : AST.expr =
  mlhs env x |> list_to_maybe_tuple

and anon_choice_lhs_3a98eae (env : env) (x : CST.anon_choice_lhs_3a98eae) =
  match x with
  | `Lhs x -> lhs env x
  | `Rest_assign x -> rest_assignment env x
  | `Dest_left_assign x -> destructured_left_assignment env x

and destructured_left_assignment (env : env)
    ((v1, v2, v3) : CST.destructured_left_assignment) =
  let _lp = token2 env v1 in
  let v2 = mlhs env v2 in
  let _rp = token2 env v3 in
  Tuple v2

and rest_assignment (env : env) ((v1, v2) : CST.rest_assignment) =
  let v1 = token2 env v1 (* "*" *) in
  let v2 =
    match v2 with
    | Some x -> Some (lhs env x)
    | None -> None
  in
  Splat (v1, v2)

and lhs (env : env) (x : CST.lhs) : AST.expr =
  match x with
  | `Var x -> variable env x
  | `True x -> Literal (Bool (true_ env x))
  | `False x -> Literal (Bool (false_ env x))
  | `Nil x -> Literal (Nil (nil env x))
  | `Scope_resol x -> ScopedId (scope_resolution env x)
  | `Elem_ref (v1, v2, v3, v4) ->
      let v1 = primary env v1 in
      let v2 = token2 env v2 in
      let v3 =
        match v3 with
        | Some x -> argument_list_with_trailing_comma env x
        | None -> []
      in
      let v4 = token2 env v4 in
      let e =
        (* THINK: Why do we need a DotAccess here rather than just `v1' ?
         *   And why a Call rather than an ArrayAccess ?
         *)
        DotAccess (v1, v2, MethodOperator (Op_AREF, v2))
      in
      Call (e, (v2, v3, v4), None)
  | `Call_ x -> call_ env x

and method_name (env : env) (x : CST.method_name) : AST.method_name =
  match x with
  | `Id tok -> MethodId (str env tok, ID_Lowercase)
  | `Cst x -> MethodId (constant env x)
  | `Setter (v1, v2) ->
      let v1 = str env v1 in
      let v2 = token2 env v2 in
      MethodIdAssign (v1, v2, ID_Lowercase)
  | `Simple_symb tok -> MethodAtom (simple_symbol env tok)
  | `Deli_symb x -> MethodAtom (delimited_symbol env x)
  | `Op x -> (
      let op = operator env x in
      match op with
      | Left bin, t -> MethodOperator (bin, t)
      | Right un, t -> MethodUOperator (un, t))
  | `Nonl_var x -> MethodId (nonlocal_variable env x)
  | `Func_id x -> MethodId (function_identifier env x, ID_Lowercase)

and interpolation (env : env) (x : CST.interpolation) :
    AST.expr AST.bracket option =
  match x with
  | `HASHLCURL_opt_stmts_RCURL (v1, v2, v3) -> (
      let lb = token2 env v1 (* "#{" *) in
      let rb = token2 env v3 (* "}" *) in
      match v2 with
      | Some x -> (
          (* Hopefully should be fine to represent these as statements.
             If these statements are typically ExprStmts, though, this
             might get sus.
          *)
          match statements env x with
          | [ e ] -> Some (lb, e, rb)
          | other -> Some (lb, S (Block (other |> fb)), rb))
      | None -> None)
  | `Short_interp_nonl_var (v1, v2) ->
      let _v1 = (* short_interpolation *) token2 env v1 in
      let v2 = nonlocal_variable env v2 in
      Some (Id v2 |> fb)

and string_ (env : env) ((v1, v2, v3) : CST.string_) : AST.interp list bracket =
  let v1 = token2 env v1 in
  (* single or double quote *)
  let v2 =
    match v2 with
    | Some x -> literal_contents env x
    | None -> []
  in
  let v3 = token2 env v3 in
  (* single or double quote *)
  (v1, v2, v3)

and simple_symbol (env : env) (tok : CST.simple_symbol) : atom =
  (* TODO: split tok *)
  let t = token2 env tok in
  let tcolon, tafter = Tok.split_tok_at_bytepos 1 t in
  let str = Tok.content_of_tok tafter in
  (tcolon, AtomSimple (str, tafter))

and delimited_symbol (env : env) ((v1, v2, v3) : CST.delimited_symbol) : atom =
  (* TODO: split v1 *)
  let v1 = token2 env v1 (* symbol_start :" "*) in
  let res =
    match v2 with
    | Some x -> literal_contents env x
    | None -> []
  in
  let v3 = token2 env v3 (* string_end " "*) in
  (Tok.fake_tok v1 ":", AtomFromString (v1, res, v3))

and literal_contents (env : env) (xs : CST.literal_contents) : AST.interp list =
  List_.filter_map
    (fun x ->
      match x with
      | `Str_content tok ->
          let x = str env tok in
          Some (StrChars x)
      | `Interp x -> (
          match interpolation env x with
          | Some (lb, e, rb) -> Some (StrExpr (lb, e, rb))
          | None -> None)
      | `Esc_seq tok ->
          let x = str env tok in
          Some (StrChars x))
    xs

and mlhs (env : env) ((v1, v2, v3) : CST.mlhs) : AST.expr list =
  let v1 = anon_choice_lhs_3a98eae env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token2 env v1 in
        let v2 = anon_choice_lhs_3a98eae env v2 in
        v2)
      v2
  in
  let _v3 =
    match v3 with
    | Some _tok -> ()
    | None -> ()
  in
  v1 :: v2

and pair (env : env) (x : CST.pair) =
  match x with
  | `Semg_ellips tok -> if_in_pattern env (Left (Ellipsis (token2 env tok)))
  | `Choice_arg_EQGT_arg x -> (
      match x with
      | `Arg_EQGT_arg (v1, v2, v3) ->
          let v1 = arg env v1 in
          let v2 = token2 env v2 in
          (* => *)
          let v3 = arg env v3 in
          (* will be converted to ArgKwd in ruby_to_generic.ml if needed *)
          Left (Binop (v1, (Op_ASSOC, v2), v3))
      | `Choice_str_imm_tok_colon_arg (v1, v2, v3) ->
          let v1 =
            match v1 with
            | `Str x -> Literal (String (mk_string_kind (string_ env x)))
          in
          let v2 = (* ":" *) token2 env v2 in
          let v3 = arg env v3 in
          Left (Binop (v1, (Op_ASSOC, v2), v3))
      | `Choice_hash_key_symb_imm_tok_colon_choice_opt_arg (v1, v2, v3) -> (
          let v1 =
            match v1 with
            | `Hash_key_symb tok -> Id (str env tok, ID_Lowercase)
            | `Id tok -> Id (str env tok, ID_Lowercase)
            | `Cst x -> Id (constant env x)
            | `Id_suffix x -> Id (identifier_suffix env x, ID_Lowercase)
            | `Cst_suffix x -> Id (constant_suffix env x, ID_Lowercase)
          in
          let v2 = token2 env v2 in
          (* : *)
          let v3 =
            match v3 with
            | `No_line_brk _v3 ->
                (* Impossible according to the tree-sitter grammar *)
                (* https://github.com/tree-sitter/tree-sitter-ruby/blob/f257f3f57833d584050336921773738a3fd8ca22/grammar.js#L1204C13-L1204C13 *)
                failwith "impossible"
            | `Opt_arg None -> fake_nil v2
            | `Opt_arg (Some v3) -> arg env v3
          in
          match v1 with
          | Id (x, _) -> Right (x, v2, v3)
          | _ -> Left (Binop (v1, (Op_ASSOC, v2), v3))))

and pair_for_hash env x : AST.expr =
  match pair env x with
  | Left e -> e
  | Right (a, b, c) -> AST.keyword_arg_to_expr a b c

and pair_for_argument env x : AST.argument =
  match pair env x with
  | Left e -> Arg e
  | Right (a, b, c) -> ArgKwd (a, b, c)

let program (env : env) ((v1, _v2interpreted) : CST.program) : AST.stmts =
  match v1 with
  | Some x -> statements env x
  | None -> []

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  let debug = false in
  H.wrap_parser
    (fun () -> Tree_sitter_ruby.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = Program } in
      if debug then Boilerplate.dump_tree cst;
      program env cst)

let parse_pattern string =
  let debug = false in
  H.wrap_parser
    (fun () -> Tree_sitter_ruby.Parse.string string)
    (fun cst _extras ->
      let file = Fpath.v "<file>" in
      let env =
        { H.file; conv = H.line_col_to_pos_pattern string; extra = Pattern }
      in
      if debug then Boilerplate.dump_tree cst;
      Ss (program env cst))
