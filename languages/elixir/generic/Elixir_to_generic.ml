(* Yoann Padioleau
 *
 * Copyright (c) 2022-2023 Semgrep Inc.
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
(* AST_elixir to generic AST conversion
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fb = Tok.unsafe_fake_bracket
let map_string _env x = x
let map_int _env x = x
let map_char _env x = x
let map_list f env xs = Common.map (f env) xs
let map_option f env x = Option.map (f env) x

let either_to_either3 = function
  | Left x -> Left3 x
  | Right (l, x, r) -> Right3 (l, Some x, r)

type quoted_generic = (string G.wrap, G.expr G.bracket) either list G.bracket
type keywords_generic = ((G.ident, quoted_generic) either * G.expr) list

type stab_clause_generic =
  (G.argument list * (Tok.t * G.expr) option) * Tok.t * G.stmt list

type body_or_clauses_generic = (G.stmt list, stab_clause_generic list) either

type do_block_generic =
  (body_or_clauses_generic
  * (exn_clause_kind wrap * body_or_clauses_generic) list)
  bracket

let expr_of_quoted (quoted : quoted_generic) : G.expr =
  let l, xs, r = quoted in
  G.interpolated (l, xs |> Common.map either_to_either3, r)

let keyval_of_pair (kwd, e) =
  let key =
    match kwd with
    | Left id -> G.N (H.name_of_id id) |> G.e
    | Right (quoted : quoted_generic) -> expr_of_quoted quoted
  in
  G.keyval key (G.fake "=>") e

let argument_of_pair (kwd, e) =
  match kwd with
  | Left id -> G.ArgKwd (id, e)
  | Right (quoted : quoted_generic) ->
      let l, _, _ = quoted in
      let e = expr_of_quoted quoted in
      OtherArg (("ArgKwdQuoted", l), [ G.E e ])

let list_container_of_kwds (kwds : keywords_generic) : G.expr =
  G.Container (G.List, fb (kwds |> Common.map keyval_of_pair)) |> G.e

let expr_of_expr_or_kwds (x : (G.expr, keywords_generic) either) : G.expr =
  match x with
  | Left e -> e
  | Right kwds -> list_container_of_kwds kwds

(* TODO: lots of work here to detect when args is really a single
 * pattern, or tuples *)
let pat_of_args_and_when (args, when_opt) : G.pattern =
  let rest =
    match when_opt with
    | None -> []
    | Some (_tok, e) -> [ G.E e ]
  in
  G.OtherPat (("ArgsAndWhenOpt", G.fake ""), G.Args args :: rest) |> G.p

let case_and_body_of_stab_clause (x : stab_clause_generic) : G.case_and_body =
  (* body can be empty *)
  let args_and_when, _tarrow, stmts = x in
  let pat = pat_of_args_and_when args_and_when in
  let stmt = G.stmt1 stmts in
  G.case_of_pat_and_stmt (pat, stmt)

(* TODO: if the list contains just one element, can be a simple lambda
 * as in 'fn (x, y) -> x + y end'. Otherwise it can be a multiple-cases
 * switch/match.
 * The first tk parameter corresponds to 'fn' for lambdas and 'do' when
 * used in a do_block.
 *)
let stab_clauses_to_function_definition tk (xs : stab_clause_generic list) :
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

let expr_of_body_or_clauses tk (x : body_or_clauses_generic) : G.expr =
  match x with
  | Left stmts ->
      (* less: use G.stmt1 instead? or get rid of fake_bracket here
       * passed down from caller? *)
      let block = G.Block (fb stmts) |> G.s in
      G.stmt_to_expr block
  | Right clauses ->
      let fdef = stab_clauses_to_function_definition tk clauses in
      G.Lambda fdef |> G.e

(* following Elixir semantic (unsugaring do/end block in keywords) *)
let kwds_of_do_block (bl : do_block_generic) : keywords_generic =
  let tdo, (body_or_clauses, extras), _tend = bl in
  (* In theory we should unsugar as "do:", and below for the
   * other kwds as "rescue:" for example, which is Elixir unsugaring semantic,
   * but then this does not play well with Analyze_pattern.ml
   * so simpler for now to unsugar as the actual keyword string without
   * the ':' suffix.
   *)
  let dokwd = Left ("do", tdo) in
  let e = expr_of_body_or_clauses tdo body_or_clauses in
  let pair1 = (dokwd, e) in
  let rest =
    extras
    |> Common.map (fun ((kind, t), body_or_clauses) ->
           let s = string_of_exn_kind kind in
           let kwd = Left (s, t) in
           let e = expr_of_body_or_clauses t body_or_clauses in
           (kwd, e))
  in
  pair1 :: rest

let args_of_do_block_opt (blopt : do_block_generic option) : G.argument list =
  match blopt with
  | None -> []
  | Some bl ->
      let kwds = kwds_of_do_block bl in
      Common.map argument_of_pair kwds

(*****************************************************************************)
(* Boilerplate *)
(*****************************************************************************)

let map_wrap f env (v1, v2) = (f env v1, v2)
let map_bracket f env (v1, v2, v3) = (v1, f env v2, v3)

let map_ident env v : G.ident =
  match v with
  | Id v ->
      let id = (map_wrap map_string) env v in
      id
  | IdEllipsis v -> ("...", v)
  | IdMetavar v ->
      let id = (map_wrap map_string) env v in
      id

let map_ident_expr env v : G.expr =
  match v with
  | IdEllipsis v -> G.Ellipsis v |> G.e
  | Id _
  | IdMetavar _ ->
      let id = map_ident env v in
      G.N (H.name_of_id id) |> G.e

let map_alias env v = (map_wrap map_string) env v

let map_wrap_operator env (op, tk) =
  match op with
  | OPin
  | ODot
  | OMatch
  | OCapture
  | OType
  | OStrictAnd
  | OStrictOr
  | OStrictNot
  | OPipeline
  | OModuleAttr
  | OLeftArrow
  | ODefault
  | ORightArrow
  | OCons
  | OWhen ->
      Right (Tok.content_of_tok tk, tk)
  | O op -> Left (op, tk)
  | OOther v ->
      let v = map_string env v in
      Right (v, tk)

let map_wrap_operator_ident env v : G.ident =
  match map_wrap_operator env v with
  | Left (_op, tk) -> (Tok.content_of_tok tk, tk)
  | Right (s, tk) -> (s, tk)

(* start of big mutually recursive functions *)

let rec map_atom env (tcolon, v2) : G.expr =
  let v2 = (map_or_quoted (map_wrap map_string)) env v2 in
  match v2 with
  | Left x -> G.L (G.Atom (tcolon, x)) |> G.e
  | Right quoted ->
      let e = expr_of_quoted quoted in
      G.OtherExpr (("AtomExpr", tcolon), [ E e ]) |> G.e

(* TODO: maybe need a 'a. for all type *)
and map_or_quoted f env v =
  match v with
  | X v ->
      let v = f env v in
      Left (f env v)
  | Quoted v -> Right (map_quoted env v)

and map_keyword env (v1, _tcolon) = map_or_quoted (map_wrap map_string) env v1

and map_quoted env v : quoted_generic =
  map_bracket
    (map_list (fun env x ->
         match x with
         | Left x -> Left (map_wrap map_string env x)
         | Right x -> Right (map_bracket map_expr env x)))
    env v

and map_arguments env (v1, v2) : G.argument list =
  let v1 = (map_list map_expr) env v1 in
  let v2 = map_keywords env v2 in
  Common.map G.arg v1 @ Common.map argument_of_pair v2

and map_items env (v1, v2) : G.expr list =
  let v1 = (map_list map_expr) env v1 in
  let v2 = map_keywords env v2 in
  v1 @ Common.map keyval_of_pair v2

and map_keywords env v = (map_list map_pair) env v

and map_pair env (v1, v2) =
  let v1 = map_keyword env v1 in
  let v2 = map_expr env v2 in
  (v1, v2)

and map_expr_or_kwds env v : (G.expr, keywords_generic) either =
  match v with
  | E v ->
      let v = map_expr env v in
      Left v
  | Kwds v ->
      let v = map_keywords env v in
      Right v

and map_expr env v : G.expr =
  match v with
  | I v -> map_ident_expr env v
  | L lit -> G.L lit |> G.e
  | A v -> map_atom env v
  | String v ->
      let v = map_quoted env v in
      expr_of_quoted v
  | Charlist v ->
      let v = map_quoted env v in
      let e = expr_of_quoted v in
      let l, _, _ = v in
      G.OtherExpr (("Charlist", l), [ G.E e ]) |> G.e
  | Sigil (ttilde, v2, v3) ->
      let v2 = map_sigil_kind env v2 in
      let v3 = (map_option (map_wrap map_string)) env v3 in
      G.OtherExpr
        ( ("Sigil", ttilde),
          v2
          @
          match v3 with
          | None -> []
          | Some id -> [ G.I id ] )
      |> G.e
  | List v ->
      let v = (map_bracket map_items) env v in
      G.Container (G.List, v) |> G.e
  | Tuple v ->
      let v = (map_bracket map_items) env v in
      G.Container (G.Tuple, v) |> G.e
  | Bits v ->
      let l, xs, r = (map_bracket map_items) env v in
      G.OtherExpr (("Bits", l), Common.map (fun e -> G.E e) xs @ [ G.Tk r ])
      |> G.e
  | Map (v1, v2, v3) -> (
      let v2 = (map_option map_astruct) env v2 in
      let l, xs, r = (map_bracket map_items) env v3 in
      match v2 with
      | None ->
          let l = Tok.combine_toks v1 [ l ] in
          G.Container (G.Dict, (l, xs, r)) |> G.e
      | Some astruct ->
          (* TODO?
             | Some (Left id) ->
                 let n = H2.name_of_id id in
                 let ty = G.TyN n |> G.t in
                 G.New (tpercent, ty, G.empty_id_info (), (l, Common.map G.arg xs, r))
                 |> G.e
          *)
          G.Call (astruct, (l, Common.map G.arg xs, r)) |> G.e)
  | Alias v ->
      (* TODO: split alias in components, and then use name_of_ids *)
      let v = map_alias env v in
      G.N (H.name_of_id v) |> G.e
  | Block v ->
      let l, body_or_clauses, _r = map_block env v in

      (* TODO: could pass a 'body_or_clauses bracket' to
       * expr_of_body_or_clauses to avoid the fake_bracket above
       *)
      expr_of_body_or_clauses l body_or_clauses
  | DotAlias (v1, tdot, v3) ->
      let e = map_expr env v1 in
      (* TODO: split alias in components, and then use name_of_ids *)
      let id = map_alias env v3 in
      G.DotAccess (e, tdot, G.FN (H.name_of_id id)) |> G.e
  | DotTuple (v1, tdot, v3) ->
      let e = map_expr env v1 in
      let items = (map_bracket map_items) env v3 in
      let tuple = G.Container (G.Tuple, items) |> G.e in
      G.DotAccess (e, tdot, G.FDynamic tuple) |> G.e
  (* only inside a Call *)
  | DotAnon (v1, tdot) ->
      let e = map_expr env v1 in
      G.OtherExpr (("DotAnon", tdot), [ G.E e ]) |> G.e
  (* only inside a Call *)
  | DotRemote v -> map_remote_dot env v
  | ModuleVarAccess (tat, v2) ->
      let e = map_expr env v2 in
      G.OtherExpr (("AttrExpr", tat), [ G.E e ]) |> G.e
  | ArrayAccess (v1, v2) ->
      let v1 = map_expr env v1 in
      let v2 = (map_bracket map_expr) env v2 in
      G.ArrayAccess (v1, v2) |> G.e
  | Call v -> map_call env v
  | UnaryOp (v1, v2) -> (
      let v1 = map_wrap_operator env v1 in
      let e = map_expr env v2 in
      match v1 with
      | Left (op, tk) -> G.opcall (op, tk) [ e ]
      | Right id ->
          let n = G.N (H.name_of_id id) |> G.e in
          G.Call (n, fb [ G.Arg e ]) |> G.e)
  | BinaryOp (v1, v2, v3) -> (
      let e1 = map_expr env v1 in
      let op = map_wrap_operator env v2 in
      let e2 = map_expr env v3 in
      match op with
      | Left (op, tk) -> G.opcall (op, tk) [ e1; e2 ]
      | Right id ->
          let n = G.N (H.name_of_id id) |> G.e in
          G.Call (n, fb ([ e1; e2 ] |> Common.map G.arg)) |> G.e)
  | OpArity (v1, tslash, v3) ->
      let id = map_wrap_operator_ident env v1 in
      let x = (map_wrap (map_option map_int)) env v3 in
      let lit = G.L (G.Int x) |> G.e in
      G.OtherExpr (("OpArity", tslash), [ G.I id; G.E lit ]) |> G.e
  | When (v1, twhen, v3) ->
      let e1 = map_expr env v1 in
      let v3 = map_expr_or_kwds env v3 in
      let e3 = expr_of_expr_or_kwds v3 in
      G.OtherExpr (("When", twhen), [ G.E e1; G.E e3 ]) |> G.e
  | Join (v1, tbar, v3) ->
      let e1 = map_expr env v1 in
      let v3 = map_expr_or_kwds env v3 in
      let e3 = expr_of_expr_or_kwds v3 in
      G.OtherExpr (("Join", tbar), [ G.E e1; G.E e3 ]) |> G.e
  | Lambda (tfn, v2, _tend) ->
      let xs = map_clauses env v2 in
      let fdef = stab_clauses_to_function_definition tfn xs in
      let fdef = { fdef with fkind = (G.LambdaKind, tfn) } in
      G.Lambda fdef |> G.e
  | Capture (tamp, v2) ->
      let e = map_expr env v2 in
      G.OtherExpr (("Capture", tamp), [ G.E e ]) |> G.e
  | ShortLambda (tamp, (l, v2, r)) ->
      let e = map_expr env v2 in
      H.set_e_range l r e;
      G.OtherExpr (("ShortLambda", tamp), [ G.E e ]) |> G.e
  | PlaceHolder (tamp, v2) ->
      let x = (map_wrap (map_option map_int)) env v2 in
      let lit = G.L (G.Int x) |> G.e in
      G.OtherExpr (("PlaceHolder", tamp), [ G.E lit ]) |> G.e
  | DeepEllipsis v ->
      let v = (map_bracket map_expr) env v in
      G.DeepEllipsis v |> G.e

and map_astruct env v = map_expr env v

and map_sigil_kind env v : G.any list =
  match v with
  | Lower (v1, v2) ->
      let c, tk = (map_wrap map_char) env v1 in
      let quoted = map_quoted env v2 in
      let id = (spf "%c" c, tk) in
      [ G.I id; G.E (expr_of_quoted quoted) ]
  | Upper (v1, v2) ->
      let c, tk = (map_wrap map_char) env v1 in
      let l, x, r = (map_bracket (map_wrap map_string)) env v2 in
      let id = (spf "%c" c, tk) in
      [ G.I id; G.E (G.L (G.String (l, x, r)) |> G.e) ]

and map_body env v : G.stmt list =
  let xs = (map_list map_expr) env v in
  xs |> Common.map G.exprstmt

and map_call env (v1, v2, v3) : G.expr =
  let e = map_expr env v1 in
  let l, args, r = (map_bracket map_arguments) env v2 in
  let v3 = (map_option map_do_block) env v3 in
  let args' = args_of_do_block_opt v3 in
  G.Call (e, (l, args @ args', r)) |> G.e

and map_remote_dot env (v1, tdot, v3) : G.expr =
  let e = map_expr env v1 in
  let v3 =
    match v3 with
    | X id_or_op ->
        let id =
          match id_or_op with
          | Left id -> map_ident env id
          | Right op -> map_wrap_operator_ident env op
        in
        X id
    | Quoted x -> Quoted x
  in
  let v3 = map_or_quoted (map_wrap map_string) env v3 in
  let fld =
    match v3 with
    | Left id -> G.FN (H.name_of_id id)
    | Right (quoted : quoted_generic) -> G.FDynamic (expr_of_quoted quoted)
  in
  G.DotAccess (e, tdot, fld) |> G.e

and map_stab_clause env (v : stab_clause) : stab_clause_generic =
  let (vargs, vwhenopt), tarrow, vbody = v in
  let args = map_arguments env vargs in
  let map_when env (twhen, v2) =
    let e = map_expr env v2 in
    (twhen, e)
  in
  let whenopt = (map_option map_when) env vwhenopt in
  let stmts = map_body env vbody in
  ((args, whenopt), tarrow, stmts)

and map_clauses env v = (map_list map_stab_clause) env v

and map_body_or_clauses env v =
  match v with
  | Body v ->
      let v = map_body env v in
      Left v
  | Clauses v ->
      let v = map_clauses env v in
      Right v

and map_do_block env (v : do_block) : do_block_generic =
  let map_tuple1 env (v1, v2) =
    let v1 = map_body_or_clauses env v1 in
    let map_tuple2 env (kind, v2) =
      let v2 = map_body_or_clauses env v2 in
      (kind, v2)
    in
    let v2 = (map_list map_tuple2) env v2 in
    (v1, v2)
  in
  (map_bracket map_tuple1) env v

and map_block env v = (map_bracket map_body_or_clauses) env v

let map_program env v : G.program = map_body env v

let map_any env v =
  match v with
  | Pr v ->
      let v = map_program env v in
      G.Pr v

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let program x =
  let env = () in
  map_program env x

let any x =
  let env = () in
  map_any env x
