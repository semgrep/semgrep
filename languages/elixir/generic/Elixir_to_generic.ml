(* Yoann Padioleau
 *
 * Copyright (C) 2023 Semgrep Inc.
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
open AST_elixir
module G = AST_generic
module H = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST_elixir to AST_generic.
 *
 * See AST_generic.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let fb = Tok.unsafe_fake_bracket
let keyval_of_kwd (k, v) = G.keyval k (G.fake "=>") v
let kwd_of_id (id : ident) : keyword = N (H.name_of_id id) |> G.e
let body_to_stmts es = es |> Common.map G.exprstmt

(* TODO: lots of work here to detect when args is really a single
 * pattern, or tuples *)
let pat_of_args_and_when (args, when_opt) : G.pattern =
  let rest =
    match when_opt with
    | None -> []
    | Some (_tok, e) -> [ G.E e ]
  in
  G.OtherPat (("ArgsAndWhenOpt", G.fake ""), G.Args args :: rest) |> G.p

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)
let case_and_body_of_stab_clause (x : stab_clause) : G.case_and_body =
  (* body can be empty *)
  let args_and_when, _tarrow, body = x in
  let pat = pat_of_args_and_when args_and_when in
  let stmts = body_to_stmts body in
  let stmt = G.stmt1 stmts in
  G.case_of_pat_and_stmt (pat, stmt)

(* TODO: if the list contains just one element, can be a simple lambda
 * as in 'fn (x, y) -> x + y end'. Otherwise it can be a multiple-cases
 * switch/match.
 * The first tk parameter corresponds to 'fn' for lambdas and 'do' when
 * used in a do_block.
 *)
let stab_clauses_to_function_definition tk (xs : stab_clause list) :
    G.function_definition =
  (* mostly a copy-paste of code to handle Function in ml_to_generic *)
  let xs = xs |> Common.map case_and_body_of_stab_clause in
  let id = G.implicit_param_id tk in
  let params = [ G.Param (G.param_of_id id) ] in
  let body_stmt =
    G.Switch (tk, Some (G.Cond (G.N (H.name_of_id id) |> G.e)), xs) |> G.s
  in
  {
    G.fparams = fb params;
    frettype = None;
    fkind = (G.Function, tk);
    fbody = G.FBStmt body_stmt;
  }

(* following Elixir semantic (unsugaring pairs) *)
let list_container_of_kwds xs =
  let es = xs |> Common.map keyval_of_kwd in
  Container (List, fb es) |> G.e

let args_of_exprs_and_keywords (es : expr list) (kwds : pair list) :
    argument list =
  let rest =
    match kwds with
    | [] -> []
    | kwds -> [ list_container_of_kwds kwds ]
  in
  Common.map G.arg (es @ rest)

let items_of_exprs_and_keywords (es : expr list) (kwds : pair list) : item list
    =
  es @ (kwds |> Common.map keyval_of_kwd)

let expr_of_body_or_clauses tk (x : body_or_clauses) : expr =
  match x with
  | Left [ e ] -> e
  | Left xs ->
      let stmts = body_to_stmts xs in
      (* less: use G.stmt1 instead? or get rid of fake_bracket here
       * passed down from caller? *)
      let block = Block (fb stmts) |> G.s in
      G.stmt_to_expr block
  | Right clauses ->
      let fdef = stab_clauses_to_function_definition tk clauses in
      Lambda fdef |> G.e

(* following Elixir semantic (unsugaring do/end block in keywords) *)
let kwds_of_do_block (bl : do_block) : pair list =
  let tdo, (body_or_clauses, extras), _tend = bl in
  let dokwd = kwd_of_id ("do:", tdo) in
  let e = expr_of_body_or_clauses tdo body_or_clauses in
  let pair1 = (dokwd, e) in
  let rest =
    extras
    |> Common.map (fun ((s, t), body_or_clauses) ->
           let kwd = kwd_of_id (s ^ ":", t) in
           let e = expr_of_body_or_clauses t body_or_clauses in
           (kwd, e))
  in
  pair1 :: rest

let expr_of_block (blk : block) : expr =
  (* TODO: could pass a 'body_or_clauses bracket' to
   * expr_of_body_or_clauses to avoid the fake_bracket above
   *)
  let l, body_or_clauses, _r = blk in
  expr_of_body_or_clauses l body_or_clauses

let args_of_do_block_opt (blopt : do_block option) : argument list =
  match blopt with
  | None -> []
  | Some bl ->
      let kwds = kwds_of_do_block bl in
      args_of_exprs_and_keywords [] kwds

let mk_call_no_parens (e : expr) (args : argument list)
    (blopt : do_block option) : call =
  Call (e, fb (args @ args_of_do_block_opt blopt)) |> G.e

let mk_call_parens (e : expr) (args : argument list bracket)
    (blopt : do_block option) : call =
  let l, xs, r = args in
  Call (e, (l, xs @ args_of_do_block_opt blopt, r)) |> G.e

let binary_call (e1 : expr) op_either (e2 : expr) : expr =
  match op_either with
  | Left id ->
      let n = N (H.name_of_id id) |> G.e in
      Call (n, fb ([ e1; e2 ] |> Common.map G.arg)) |> G.e
  | Right op -> G.opcall op [ e1; e2 ]

let expr_of_e_or_kwds (x : (expr, pair list) either) : expr =
  match x with
  | Left e -> e
  | Right kwds -> list_container_of_kwds kwds
