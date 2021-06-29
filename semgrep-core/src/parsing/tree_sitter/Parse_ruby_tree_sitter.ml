(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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
module AST = Ast_ruby
module CST = Tree_sitter_ruby.CST
module PI = Parse_info
open Ast_ruby
module G = AST_generic
module H = Parse_tree_sitter_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ruby parser using tree-sitter-lang/semgrep-ruby and converting
 * to pfff/lang_ruby/parsing/ast_ruby.ml
 *
 * The resulting AST can then be converted to the generic AST by using
 * pfff/lang_ruby/analyze/ruby_to_generic.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type env = unit H.env

let _todo (_env : env) _ = failwith "not implemented, yikes."

let fb = PI.fake_bracket

let list_to_maybe_tuple = function
  | [] -> raise Impossible
  | [ x ] -> x
  | xs -> Tuple xs

let mk_Literal_String (t1, xs, t2) =
  let string_kind =
    match (PI.str_of_info t1, xs) with
    | "'", [] -> Single ("", PI.combine_infos t1 [ t2 ])
    | "'", [ StrChars (s, t) ] -> Single (s, PI.combine_infos t1 [ t; t2 ])
    | _ -> Double (t1, xs, t2)
  in
  Literal (String string_kind)

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
  | `TILDE tok -> (Right Op_UTilde, token2 env tok)
  | `BANG tok -> (Right Op_UBang, token2 env tok)
  (* TODO *)
  | `BQUOT tok ->
      pr2_gen tok;
      failwith "Op_BQUOT???"

let anon_choice_int_e7b97da (env : env) (x : CST.anon_choice_int_e7b97da) =
  match x with
  | `Int tok ->
      str env tok
      (* pattern 0[bB][01](_?[01])*|0[oO]?[0-7](_?[0-7])*|(0[dD])?\d(_?\d)*|0x[0-9a-fA-F](_?[0-9a-fA-F])* *)
  | `Float tok -> str env tok

(* pattern \d(_?\d)*(\.\d)?(_?\d)*([eE][\+-]?\d(_?\d)*\
   )? *)

let anon_choice_DOT_5431c66 (env : env) (x : CST.anon_choice_DOT_5431c66) =
  match x with `DOT tok -> token2 env tok | `AMPDOT tok -> token2 env tok

let terminator (_env : env) (x : CST.terminator) : unit =
  match x with `Line_brk _tok -> () | `SEMI _tok -> ()

let variable (env : env) (x : CST.variable) : AST.variable =
  match x with
  (* TODO: move this to variable type *)
  | `Self tok -> (str env tok, ID_Self)
  | `Super tok -> (str env tok, ID_Super)
  | `Inst_var tok -> (str env tok, ID_Instance)
  | `Class_var tok -> (str env tok, ID_Class)
  | `Global_var tok -> (str env tok, ID_Global)
  | `Id tok -> (str env tok, ID_Lowercase)
  | `Cst tok -> (str env tok, ID_Uppercase)

let rec statements (env : env) (x : CST.statements) : AST.stmts =
  match x with
  | `Rep1_choice_stmt_term_opt_stmt (v1, v2) ->
      let v1 =
        v1
        |> List.map (fun x ->
               match x with
               | `Stmt_term (v1, v2) ->
                   let v1 = statement env v1 in
                   let _v2 = terminator env v2 in
                   [ v1 ]
               (* TODO? use EmptyStmt in generic AST? *)
               | `Empty_stmt _tok -> [])
        |> List.flatten
      in
      let v2 = match v2 with Some x -> [ statement env x ] | None -> [] in
      v1 @ v2
  | `Stmt x -> [ statement env x ]

and statement (env : env) (x : CST.statement) :
    AST.expr (* TODO AST.stmt at some point *) =
  match x with
  | `Undef (v1, v2, v3) ->
      let v1 = token2 env v1 in
      let v2 = method_name env v2 in
      let v3 =
        List.map
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
      let v3 = match v3 with Some x -> statements env x | None -> [] in
      let v4 = token2 env v4 in
      D (BeginBlock (v1, (v2, v3, v4)))
  | `End_blk (v1, v2, v3, v4) ->
      let v1 = token2 env v1 in
      let v2 = token2 env v2 in
      let v3 = match v3 with Some x -> statements env x | None -> [] in
      let v4 = token2 env v4 in
      D (EndBlock (v1, (v2, v3, v4)))
  | `Exp x -> expression env x

and method_rest (env : env) ((v1, v2, v3) : CST.method_rest) =
  let v1 = method_name env v1 in
  let v2 =
    match v2 with
    | `Params_opt_term (v1, v2) ->
        let v1 = parameters env v1 |> G.unbracket in
        let _v2 = match v2 with Some x -> terminator env x | None -> () in
        v1
    | `Opt_bare_params_term (v1, v2) ->
        let v1 = match v1 with Some x -> bare_parameters env x | None -> [] in
        let _v2 = terminator env v2 in
        v1
  in
  let xs, _tend = body_statement env v3 in
  (v1, v2, xs)

and parameters (env : env) ((v1, v2, v3) : CST.parameters) :
    AST.formal_param list bracket =
  let lp = token2 env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = formal_parameter env v1 in
        let v2 =
          List.map
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
    List.map
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
          List.map
            (fun (v1, v2) ->
              let _v1 = token2 env v1 in
              let v2 = formal_parameter env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 = match v3 with Some _tok -> () | None -> () in
  let _v4SEMICOLONPARAMS =
    match v4 with
    | Some (v1, v2, v3) ->
        let _v1 = token2 env v1 in
        let v2 = str env v2 in
        let v3 =
          List.map
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
      | None -> Formal_rest v1 )
  | `Hash_splat_param (v1, v2) ->
      let v1 = token2 env v1 in
      let v2 = match v2 with Some tok -> Some (str env tok) | None -> None in
      Formal_hash_splat (v1, v2)
  | `Blk_param (v1, v2) ->
      let v1 = token2 env v1 in
      let v2 = str env v2 in
      Formal_amp (v1, v2)
  | `Kw_param (v1, v2, v3) ->
      let v1 = str env v1 in
      let v2 = token2 env v2 in
      let v3 = match v3 with Some x -> Some (arg env x) | None -> None in
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
    List.map
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
  match x with `Arg x -> arg env x | `Splat_arg x -> splat_argument env x

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
          ( match x with
          | `Else x -> else_ env x
          | `Elsif x ->
              let t, s = elsif env x in
              (t, [ S s ]) )
    | None -> None
  in
  (v1, If (v1, v2, v3, v4))

and else_ (env : env) ((v1, v2, v3) : CST.else_) : AST.tok * AST.stmts =
  let v1 = token2 env v1 in
  let _v2 = match v2 with Some x -> terminator env x | None -> () in
  let v3 = match v3 with Some x -> statements env x | None -> [] in
  (v1, v3)

and then_ (env : env) (x : CST.then_) : AST.stmts =
  match x with
  | `Term_stmts (v1, v2) ->
      let _v1 = terminator env v1 in
      let v2 = statements env v2 in
      v2
  | `Opt_term_then_opt_stmts (v1, v2, v3) ->
      let _v1 = match v1 with Some x -> terminator env x | None -> () in
      let _v2 = token2 env v2 in
      let v3 = match v3 with Some x -> statements env x | None -> [] in
      v3

and ensure (env : env) ((v1, v2) : CST.ensure) =
  let v1 = token2 env v1 in
  let v2 = match v2 with Some x -> statements env x | None -> [] in
  (v1, v2)

and rescue (env : env) ((v1, v2, v3, v4) : CST.rescue) =
  let v1 = token2 env v1 in
  let v2 = match v2 with Some x -> exceptions env x | None -> [] in
  let v3 =
    match v3 with Some x -> Some (exception_variable env x) | None -> None
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
    match v1 with `Arg x -> arg env x | `Splat_arg x -> splat_argument env x
  in
  let v2 =
    List.map
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

and body_statement (env : env) ((v1, v2, v3) : CST.body_statement) :
    AST.body_exn * AST.tok =
  let v1 = match v1 with Some x -> statements env x | None -> [] in
  let rescue_exprs, else_expr, ensure_expr =
    Common2.partition_either3
      (fun x ->
        match x with
        | `Rescue x -> Common2.Left3 (rescue env x)
        | `Else x -> Common2.Middle3 (else_ env x)
        | `Ensure x ->
            let t, xs = ensure env x in
            Common2.Right3 (t, xs))
      v2
  in
  let tend = token2 env v3 in
  let ensure_expr =
    match ensure_expr with [] -> None | [ x ] -> Some x | x :: _ -> Some x
    (* TODO: weird, should have only one no? *)
  in
  let else_expr =
    match else_expr with [] -> None | [ x ] -> Some x | x :: _ -> Some x
    (* TODO: weird, should have only one no? *)
  in
  ({ body_exprs = v1; rescue_exprs; else_expr; ensure_expr }, tend)

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
      let v3 = expression env v3 in
      Binop (v1, (Op_OP_ASGN op, tok), v3)
  | `Cmd_call (v1, v2) ->
      let v1 =
        match v1 with
        | `Call x -> call env x
        | `Chai_cmd_call x -> chained_command_call env x
        | `Choice_var x -> (
            match x with
            | `Var x -> Id (variable env x)
            | `Scope_resol x -> ScopedId (scope_resolution env x) )
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
  | `Arg x -> arg env x

and arg (env : env) (x : CST.arg) : AST.expr =
  match x with
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
      let v3 = arg env v3 in
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

and anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e (env : env)
    ((v1, v2) : CST.anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e) =
  let v1 = literal_contents env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token2 env v1 (* pattern \s+ *) in
        let v2 = literal_contents env v2 in
        v2)
      v2
  in
  v1 :: v2

and primary (env : env) (x : CST.primary) : AST.expr =
  match x with
  | `Paren_stmts x ->
      let lp, xs, rp = parenthesized_statements env x in
      S (Block (lp, xs, rp))
  | `Lhs x -> lhs env x
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
  | `Str_array (v1, v2, v3, v4, v5) ->
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
        match v4 with Some tok -> Some (token2 env tok) | None -> None
      in
      let v5 = token2 env v5 (* string_end *) in
      Literal (String (Double (v1, v3 |> List.flatten, v5)))
  | `Symb_array (v1, v2, v3, v4, v5) ->
      let v1 = token2 env v1 (* %i( *) in
      let _v2 =
        match v2 with Some tok -> Some (token2 env tok) | None -> None
      in
      let v3 =
        match v3 with
        | Some x -> anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e env x
        | None -> []
      in
      let _v4 =
        match v4 with Some tok -> Some (token2 env tok) | None -> None
      in
      let v5 = token2 env v5 (* ) *) in
      Atom (v1, AtomFromString (v1, v3 |> List.flatten, v5))
  | `Hash (v1, v2, v3) ->
      let v1 = token2 env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2, v3) ->
            let v1 =
              match v1 with
              | `Pair x -> pair env x
              | `Hash_splat_arg x -> hash_splat_argument env x
            in
            let v2 =
              List.map
                (fun (v1, v2) ->
                  let _v1 = token2 env v1 in
                  let v2 =
                    match v2 with
                    | `Pair x -> pair env x
                    | `Hash_splat_arg x -> hash_splat_argument env x
                  in
                  v2)
                v2
            in
            let _v3 = match v3 with Some _tok -> () | None -> () in
            v1 :: v2
        | None -> []
      in
      let v3 = token2 env v3 in
      Hash (true, (v1, v2, v3))
  | `Subs (v1, v2, v3) ->
      let v1 = token2 env v1 in
      let v2 = match v2 with Some x -> literal_contents env x | None -> [] in
      let v3 = token2 env v3 in
      Literal (String (Tick (v1, v2, v3)))
  | `Simple_symb tok -> Atom (simple_symbol env tok)
  | `Deli_symb x -> Atom (delimited_symbol env x)
  | `Int tok -> Literal (Num (str env tok))
  | `Float tok -> Literal (Float (str env tok))
  | `Comp tok -> Literal (Complex (str env tok))
  | `Rati (v1, v2) ->
      let v1 = anon_choice_int_e7b97da env v1 in
      let v2 = token2 env v2 in
      Literal (Rational (v1, v2))
  | `Str x -> mk_Literal_String (string_ env x)
  | `Char tok -> Literal (Char (str env tok))
  (* ??? *)
  | `Chai_str (v1, v2) ->
      let l, v1, r = string_ env v1 in
      let v2 =
        List.map
          (fun x ->
            let _lp, x, _ = string_ env x in
            x)
          v2
        |> List.flatten
      in
      Literal (String (Double (l, v1 @ v2, r)))
  | `Regex (v1, v2, v3) ->
      let v1 = token2 env v1 in
      let v2 = match v2 with Some x -> literal_contents env x | None -> [] in
      let v3 = token2 env v3 in
      (* TODO: no modifier in Ruby grammar.js? *)
      Literal (Regexp ((v1, v2, v3), None))
  | `Lambda (v1, v2, v3) ->
      let v1 = token2 env v1 in
      let v2 =
        match v2 with
        | Some x ->
            Some
              ( match x with
              | `Params x -> parameters env x |> G.unbracket
              | `Bare_params x -> bare_parameters env x )
        | None -> None
      in
      let v3 =
        match v3 with `Blk x -> block env x | `Do_blk x -> do_block env x
      in
      let b = false in
      (* should have a third option for lambdas *)
      CodeBlock ((v1, b, v1), v2, [ v3 ])
  | `Meth (v1, v2) ->
      let v1 = token2 env v1 in
      let n, params, body_exn = method_rest env v2 in
      D (MethodDef (v1, M n, params, body_exn))
  | `Sing_meth (v1, v2, v3, v4) ->
      let v1 = token2 env v1 in
      let v2 =
        match v2 with
        | `Var x -> Id (variable env x)
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
        | `Cst tok -> NameConstant (str env tok)
        | `Scope_resol x -> NameScope (scope_resolution env x)
      in
      let v3 =
        match v3 with Some x -> Some (superclass env x) | None -> None
      in
      let _v4 = terminator env v4 in
      let v5, _tend = body_statement env v5 in
      D (ClassDef (v1, C (v2, v3), v5))
  | `Sing_class (v1, v2, v3, v4, v5) ->
      let v1 = token2 env v1 in
      let v2 = token2 env v2 in
      let v3 = arg env v3 in
      let _v4 = terminator env v4 in
      let v5, _tend = body_statement env v5 in
      D (ClassDef (v1, SingletonC (v2, v3), v5))
  | `Module (v1, v2, v3) ->
      let v1 = token2 env v1 in
      let v2 =
        match v2 with
        | `Cst tok -> NameConstant (str env tok)
        | `Scope_resol x -> NameScope (scope_resolution env x)
      in
      let v3, _tend =
        match v3 with
        | `Term_body_stmt (v1, v2) ->
            let _v1 = terminator env v1 in
            let v2 = body_statement env v2 in
            v2
        | `End tok -> (empty_body_exn, token2 env tok)
      in
      D (ModuleDef (v1, v2, v3))
  | `Begin (v1, v2, v3) ->
      let tbegin = token2 env v1 in
      let _v2 = match v2 with Some x -> terminator env x | None -> () in
      let v3, tend = body_statement env v3 in
      S (Block (tbegin, [ S (ExnBlock v3) ], tend))
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
              ( match x with
              | `Else x -> else_ env x
              | `Elsif x ->
                  let t, s = elsif env x in
                  (t, [ S s ]) )
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
              ( match x with
              | `Else x -> else_ env x
              | `Elsif x ->
                  let t, s = elsif env x in
                  (t, [ S s ]) )
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
      S (For (v1, v2, t, e, v4))
  | `Case (v1, v2, v3, v5, v6, v7) ->
      let v1 = token2 env v1 in
      let v2 =
        match v2 with Some x -> Some (statement env x) | None -> None
      in
      let _v3 =
        match v3 with Some x -> Some (terminator env x) | None -> None
      in
      let v5 = List.map (when_ env) v5 in
      let v6 = match v6 with Some x -> Some (else_ env x) | None -> None in
      let _v7 = token2 env v7 in
      S (Case (v1, { case_guard = v2; case_whens = v5; case_else = v6 }))
  | `Ret (v1, v2) ->
      let v1 = token2 env v1 in
      let v2 =
        match v2 with
        | Some x -> argument_list env x |> G.unbracket
        | None -> []
      in
      S (Return (v1, v2))
  | `Yield (v1, v2) ->
      let v1 = token2 env v1 in
      let v2 =
        match v2 with
        | Some x -> argument_list env x |> G.unbracket
        | None -> []
      in
      S (Yield (v1, v2))
  | `Brk (v1, v2) ->
      let v1 = token2 env v1 in
      let v2 =
        match v2 with
        | Some x -> argument_list env x |> G.unbracket
        | None -> []
      in
      S (Break (v1, v2))
  | `Next (v1, v2) ->
      let v1 = token2 env v1 in
      let v2 =
        match v2 with
        | Some x -> argument_list env x |> G.unbracket
        | None -> []
      in
      S (Next (v1, v2))
  | `Redo (v1, v2) ->
      let v1 = token2 env v1 in
      let v2 =
        match v2 with
        | Some x -> argument_list env x |> G.unbracket
        | None -> []
      in
      S (Redo (v1, v2))
  | `Retry (v1, v2) ->
      let v1 = token2 env v1 in
      let v2 =
        match v2 with
        | Some x -> argument_list env x |> G.unbracket
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
  | `Un_lit (v1, v2) ->
      let v1 =
        match v1 with
        | `Un_minus tok -> (U Op_UMinus, token2 env tok)
        | `PLUS tok -> (U Op_UPlus, token2 env tok)
      in
      let v2 =
        match v2 with
        | `Int tok -> Literal (Num (str env tok))
        | `Float tok -> Literal (Float (str env tok))
      in
      Unary (v1, v2)
  | `Here_begin tok ->
      let x = str env tok in
      Literal (String (Single x))

and parenthesized_statements (env : env)
    ((v1, v2, v3) : CST.parenthesized_statements) =
  let v1 = token2 env v1 in
  let v2 = match v2 with Some x -> statements env x | None -> [] in
  let v3 = token2 env v3 in
  (v1, v2, v3)

and scope_resolution (env : env) ((v1, v2) : CST.scope_resolution) :
    AST.scope_resolution =
  let v1 =
    match v1 with
    | `COLONCOLON tok -> fun e -> TopScope (token2 env tok, e)
    | `Prim_imm_tok_COLONCOLON (v1, v2) ->
        let v1 = primary env v1 in
        let v2 = token2 env v2 in
        fun e -> Scope (v1, v2, SV e)
  in
  let v2 =
    match v2 with
    | `Id tok -> (str env tok, ID_Lowercase)
    | `Cst tok -> (str env tok, ID_Uppercase)
  in
  v1 v2

and anon_choice_id_5ca805c (env : env) (x : CST.anon_choice_id_5ca805c) =
  match x with
  | `Id tok -> MethodId (str env tok, ID_Lowercase)
  | `Op x -> (
      let op = operator env x in
      match op with
      | Left bin, t -> MethodOperator (bin, t)
      | Right un, t -> MethodUOperator (un, t) )
  | `Cst tok -> MethodId (str env tok, ID_Uppercase)
  | `Arg_list x ->
      (* ?? *)
      MethodDynamic (Tuple (argument_list env x |> G.unbracket))

and call (env : env) ((v1, v2, v3) : CST.call) =
  let v1 = primary env v1 in
  let v2 = anon_choice_DOT_5431c66 env v2 in
  let v3 = anon_choice_id_5ca805c env v3 in
  DotAccess (v1, v2, v3)

and chained_command_call (env : env) ((v1, v2, v3) : CST.chained_command_call) :
    AST.expr =
  let v1 = command_call_with_block env v1 in
  let v2 = anon_choice_DOT_5431c66 env v2 in
  let v3 = anon_choice_id_5ca805c env v3 in
  DotAccess (v1, v2, v3)

and command_call_with_block (env : env) (x : CST.command_call_with_block) :
    AST.expr =
  match x with
  | `Choice_call_cmd_arg_list_blk (v1, v2, v3) ->
      let v1 = anon_choice_call_fd54051 env v1 in
      let v2 = command_argument_list env v2 in
      let v3 = block env v3 in
      Call (v1, fb v2, Some v3)
  | `Choice_call_cmd_arg_list_do_blk (v1, v2, v3) ->
      let v1 = anon_choice_call_fd54051 env v1 in
      let v2 = command_argument_list env v2 in
      let v3 = do_block env v3 in
      Call (v1, fb v2, Some v3)

and anon_choice_call_fd54051 (env : env) (x : CST.anon_choice_call_fd54051) =
  match x with
  | `Call x -> call env x
  | `Choice_var x -> map_anon_choice_var_18b08b3 env x

and map_anon_choice_var_18b08b3 (env : env) (x : CST.anon_choice_var_18b08b3) =
  match x with
  | `Var x -> Id (variable env x)
  | `Scope_resol x -> ScopedId (scope_resolution env x)

and call_ (env : env) (x : CST.call_) : AST.expr =
  match x with
  | `Choice_call_arg_list (v1, v2) ->
      let v1 = anon_choice_call_fd54051 env v1 in
      let v2 = argument_list env v2 in
      Call (v1, v2, None)
  | `Choice_call_arg_list_blk (v1, v2, v3) ->
      let v1 = anon_choice_call_fd54051 env v1 in
      let v2 = argument_list env v2 in
      let v3 = block env v3 in
      Call (v1, v2, Some v3)
  | `Choice_call_arg_list_do_blk (v1, v2, v3) ->
      let v1 = anon_choice_call_fd54051 env v1 in
      let v2 = argument_list env v2 in
      let v3 = do_block env v3 in
      Call (v1, v2, Some v3)
  | `Choice_call_blk (v1, v2) ->
      let v1 = anon_choice_call_fd54051 env v1 in
      let v2 = block env v2 in
      Call (v1, fb [], Some v2)
  | `Choice_call_do_blk (v1, v2) ->
      let v1 = anon_choice_call_fd54051 env v1 in
      let v2 = do_block env v2 in
      Call (v1, fb [], Some v2)

and command_argument_list (env : env) ((v1, v2) : CST.command_argument_list) :
    AST.expr list =
  let v1 = argument env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _t = token2 env v1 in
        let v2 = argument env v2 in
        v2)
      v2
  in
  v1 :: v2

and argument_list (env : env) ((v1, v2, v3) : CST.argument_list) :
    AST.expr list AST.bracket =
  let lp = token2 env v1 in
  let v2 =
    match v2 with
    | Some x -> argument_list_with_trailing_comma env x
    | None -> []
  in
  let rp = token2 env v3 in
  (lp, v2, rp)

and argument_list_with_trailing_comma (env : env)
    ((v1, v2, v3) : CST.argument_list_with_trailing_comma) : AST.expr list =
  let v1 = argument env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _ = token2 env v1 in
        let v2 = argument env v2 in
        v2)
      v2
  in
  let _v3 = match v3 with Some _tok -> () | None -> () in
  v1 :: v2

and argument (env : env) (x : CST.argument) : AST.expr =
  match x with
  | `Exp x -> expression env x
  | `Splat_arg x -> splat_argument env x
  | `Hash_splat_arg x -> hash_splat_argument env x
  | `Blk_arg (v1, v2) ->
      let v1 = token2 env v1 in
      let v2 = arg env v2 in
      Unary ((Op_UAmper, v1), v2)
  | `Pair x -> pair env x

and splat_argument (env : env) ((v1, v2) : CST.splat_argument) =
  let v1 = token2 env v1 in
  let v2 = arg env v2 in
  Splat (v1, Some v2)

and hash_splat_argument (env : env) ((v1, v2) : CST.hash_splat_argument) =
  let v1 = token2 env v1 (* hash_splat_star_star *) in
  let v2 = arg env v2 in
  Unary ((Op_UStarStar, v1), v2)

and do_ (env : env) ((v1, v2, v3) : CST.do_) : AST.expr list =
  (match v1 with `Do _tok (* "do" *) -> () | `Term x -> terminator env x);
  let v2 = match v2 with Some x -> statements env x | None -> [] in
  let _v3 = token2 env v3 (* "end" *) in
  v2

and do_block (env : env) ((v1, v2, v3, v4) : CST.do_block) : AST.expr =
  let tdo = token2 env v1 in
  let _v2 = match v2 with Some x -> terminator env x | None -> () in
  let params_opt =
    match v3 with
    | Some (v1, v2) ->
        let v1 = block_parameters env v1 |> G.unbracket in
        let _v2 = match v2 with Some x -> terminator env x | None -> () in
        Some v1
    | None -> None
  in
  let exn_block, tend = body_statement env v4 in
  let xs = [ S (ExnBlock exn_block) ] in
  CodeBlock ((tdo, false, tend), params_opt, xs)

and block (env : env) ((v1, v2, v3, v4) : CST.block) =
  let lb = token2 env v1 in
  let params_opt =
    match v2 with
    | Some x -> Some (block_parameters env x |> G.unbracket)
    | None -> None
  in
  let v3 = match v3 with Some x -> statements env x | None -> [] in
  let rb = token2 env v4 in
  CodeBlock ((lb, true, rb), params_opt, v3)

and assignment (env : env) (x : CST.assignment) =
  match x with
  | `Choice_lhs_EQ_choice_arg (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Lhs x -> lhs env x
        | `Left_assign_list x -> left_assignment_list env x
      in
      let v2 = token2 env v2 in
      let v3 =
        match v3 with
        | `Arg x -> arg env x
        | `Splat_arg x -> splat_argument env x
        | `Right_assign_list x ->
            right_assignment_list env x |> list_to_maybe_tuple
      in
      Binop (v1, (Op_ASSIGN, v2), v3)

and command_assignment (env : env) (x : CST.command_assignment) =
  match x with
  | `Choice_lhs_EQ_exp (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Lhs x -> lhs env x
        | `Left_assign_list x -> left_assignment_list env x
      in
      let v2 = token2 env v2 in
      let v3 = expression env v3 in
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
  match x with
  | `Arg_choice_DOTDOT_arg (v1, v2, v3) ->
      let v1 = arg env v1 in
      let v2 = anon_choice_DOTDOT_ed078ec env v2 in
      let v3 = arg env v3 in
      Binop (v1, v2, v3)
  | `Choice_DOTDOT_arg (v1, v2) ->
      let v1 = anon_choice_DOTDOT_ed078ec env v1 in
      let v2 = arg env v2 in
      let t = snd v1 in
      Binop (fake_nil t, v1, v2)
  | `Arg_choice_DOTDOT (v1, v2) ->
      let v1 = arg env v1 in
      let v2 = anon_choice_DOTDOT_ed078ec env v2 in
      let t = snd v2 in
      Binop (v1, v2, fake_nil t)

and right_assignment_list (env : env) ((v1, v2) : CST.right_assignment_list) :
    AST.expr list =
  let v1 =
    match v1 with `Arg x -> arg env x | `Splat_arg x -> splat_argument env x
  in
  let v2 =
    List.map
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
  let v2 = match v2 with Some x -> Some (lhs env x) | None -> None in
  Splat (v1, v2)

and lhs (env : env) (x : CST.lhs) : AST.expr =
  match x with
  | `Var x -> Id (variable env x)
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
      let e = DotAccess (v1, v2, MethodOperator (Op_AREF, v4)) in
      Call (e, fb v3, None)
  | `Call x -> call env x
  | `Call_ x -> call_ env x

and method_name (env : env) (x : CST.method_name) : AST.method_name =
  match x with
  | `Id tok -> MethodId (str env tok, ID_Lowercase)
  | `Cst tok -> MethodId (str env tok, ID_Uppercase)
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
      | Right un, t -> MethodUOperator (un, t) )
  | `Inst_var tok -> MethodId (str env tok, ID_Instance)
  | `Class_var tok -> MethodId (str env tok, ID_Class)
  | `Global_var tok -> MethodId (str env tok, ID_Global)

and interpolation (env : env) ((v1, v2, v3) : CST.interpolation) :
    AST.expr AST.bracket option =
  let lb = token2 env v1 (* "#{" *) in
  let rb = token2 env v3 (* "}" *) in
  match v2 with Some x -> Some (lb, statement env x, rb) | None -> None

and string_ (env : env) ((v1, v2, v3) : CST.string_) : AST.interp list bracket =
  let v1 = token2 env v1 in
  (* single or double quote *)
  let v2 = match v2 with Some x -> literal_contents env x | None -> [] in
  let v3 = token2 env v3 in
  (* single or double quote *)
  (v1, v2, v3)

and simple_symbol (env : env) (tok : CST.simple_symbol) : atom =
  (* TODO: split tok *)
  let t = token2 env tok in
  let tcolon, tafter = PI.split_info_at_pos 1 t in
  let str = PI.str_of_info tafter in
  (tcolon, AtomSimple (str, tafter))

and delimited_symbol (env : env) ((v1, v2, v3) : CST.delimited_symbol) : atom =
  (* TODO: split v1 *)
  let v1 = token2 env v1 (* symbol_start :" "*) in
  let res = match v2 with Some x -> literal_contents env x | None -> [] in
  let v3 = token2 env v3 (* string_end " "*) in
  (Parse_info.fake_info ":", AtomFromString (v1, res, v3))

and literal_contents (env : env) (xs : CST.literal_contents) : AST.interp list =
  List.filter_map
    (fun x ->
      match x with
      | `Str_content tok ->
          let x = str env tok in
          Some (StrChars x)
      | `Interp x -> (
          match interpolation env x with
          | Some (lb, e, rb) -> Some (StrExpr (lb, e, rb))
          | None -> None )
      | `Esc_seq tok ->
          let x = str env tok in
          Some (StrChars x))
    xs

and mlhs (env : env) ((v1, v2, v3) : CST.mlhs) : AST.expr list =
  let v1 = anon_choice_lhs_3a98eae env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token2 env v1 in
        let v2 = anon_choice_lhs_3a98eae env v2 in
        v2)
      v2
  in
  let _v3 = match v3 with Some _tok -> () | None -> () in
  v1 :: v2

and pair (env : env) (x : CST.pair) =
  match x with
  | `Arg_EQGT_arg (v1, v2, v3) ->
      let v1 = arg env v1 in
      let v2 = token2 env v2 in
      (* => *)
      let v3 = arg env v3 in
      Binop (v1, (Op_ASSOC, v2), v3)
  | `Choice_hash_key_symb_imm_tok_COLON_arg (v1, v2, v3) -> (
      let v1 =
        match v1 with
        | `Hash_key_symb tok -> Id (str env tok, ID_Lowercase)
        | `Id tok -> Id (str env tok, ID_Lowercase)
        | `Cst tok -> Id (str env tok, ID_Uppercase)
        | `Str x -> mk_Literal_String (string_ env x)
      in
      let v2 = token2 env v2 in
      (* : *)
      let v3 = arg env v3 in
      match v1 with
      | Id (x, _) -> AST.keyword_arg_to_expr x v2 v3
      | _ -> Binop (v1, (Op_ASSOC, v2), v3) )

let program (env : env) ((v1, _v2interpreted) : CST.program) : AST.stmts =
  match v1 with Some x -> statements env x | None -> []

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  let debug = false in
  H.wrap_parser
    (fun () ->
      (* TODO: tree-sitter bindings are buggy so we cheat and fork to
       * avoid segfaults to popup. See Main.ml test_parse_ruby function.
       *)
      match 2 with
      (* segfault quite often *)
      | 1 -> Tree_sitter_ruby.Parse.file file
      (* segfault less, but as we fork from a more complex point where
       * we allocated quite a few stuff, the probability to get a segfault
       * in the child grows
       *)
      | _ ->
          Parallel.backtrace_when_exn := false;
          Parallel.invoke Tree_sitter_ruby.Parse.file file ())
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      ( if debug then
        let sexp = CST.sexp_of_program cst in
        let s = Sexplib.Sexp.to_string_hum sexp in
        print_endline s );
      program env cst)
