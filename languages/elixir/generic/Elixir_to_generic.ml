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

(* TODO: to remove! *)
[@@@warning "-27"]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST_elixir to generic AST conversion
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let todo _env _v = failwith "TODO"
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

let list_container_of_kwds (kwds : keywords_generic) : G.expr =
  G.Container (G.List, fb (kwds |> Common.map keyval_of_pair)) |> G.e

let expr_of_expr_or_kwds (x : (G.expr, keywords_generic) either) : G.expr =
  match x with
  | Left e -> e
  | Right kwds -> list_container_of_kwds kwds

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

and map_keyword env (v1, v2) = map_or_quoted (map_wrap map_string) env v1

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
  Common.map G.arg v1
  @ Common.map
      (fun (kwd, e) ->
        match kwd with
        | Left id -> G.ArgKwd (id, e)
        | Right (quoted : quoted_generic) ->
            let l, _, _ = quoted in
            let e = expr_of_quoted quoted in
            OtherArg (("ArgKwdQuoted", l), [ G.E e ]))
      v2

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
      let v = map_block env v in
      todo env v
  | DotAlias (v1, tdot, v3) ->
      let e = map_expr env v1 in
      (* TODO: split alias in components, and then use name_of_ids *)
      let id = map_alias env v3 in
      G.DotAccess (e, tdot, G.FN (H.name_of_id id)) |> G.e
  | DotTuple (v1, tdot, v3) ->
      let e = map_expr env v1 in
      let items = (map_bracket map_items) env v3 in
      let tuple = G.Container (G.Tuple, items) |> G.e in
      DotAccess (e, tdot, G.FDynamic tuple) |> G.e
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
          let n = N (H.name_of_id id) |> G.e in
          Call (n, fb [ G.Arg e ]) |> G.e)
  | BinaryOp (v1, v2, v3) -> (
      let e1 = map_expr env v1 in
      let op = map_wrap_operator env v2 in
      let e2 = map_expr env v3 in
      match op with
      | Left (op, tk) -> G.opcall (op, tk) [ e1; e2 ]
      | Right id ->
          let n = N (H.name_of_id id) |> G.e in
          Call (n, fb ([ e1; e2 ] |> Common.map G.arg)) |> G.e)
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
  | Lambda (v1, v2, v3) ->
      let v2 = map_clauses env v2 in
      todo env (v1, v2, v3)
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
      let v1 = (map_wrap map_char) env v1 in
      let v2 = map_quoted env v2 in
      todo env (v1, v2)
  | Upper (v1, v2) ->
      let v1 = (map_wrap map_char) env v1 in
      let v2 = (map_bracket (map_wrap map_string)) env v2 in
      todo env (v1, v2)

and map_body env v : G.stmt list =
  let xs = (map_list map_expr) env v in
  xs |> Common.map G.exprstmt

and map_call env (v1, v2, v3) : G.expr =
  let v1 = map_expr env v1 in
  let _l, _xs, _r = (map_bracket map_arguments) env v2 in
  let v3 = (map_option map_do_block) env v3 in
  todo env (v1, v2, v3)

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
  DotAccess (e, tdot, fld) |> G.e

and map_stab_clause env (v1, v2, v3) =
  let map_tuple1 env (v1, v2) =
    let v1 = map_arguments env v1 in
    let map_tuple2 env (v1, v2) =
      let v2 = map_expr env v2 in
      todo env (v1, v2)
    in
    let v2 = (map_option map_tuple2) env v2 in
    todo env (v1, v2)
  in
  let v1 = map_tuple1 env v1 in
  let v3 = map_body env v3 in
  todo env (v1, v2, v3)

and map_clauses env v = (map_list map_stab_clause) env v

and map_body_or_clauses env v =
  match v with
  | Body v ->
      let v = map_body env v in
      Left v
  | Clauses v ->
      let v = map_clauses env v in
      Right v

and map_do_block env v =
  let map_tuple1 env (v1, v2) =
    let v1 = map_body_or_clauses env v1 in
    let map_tuple2 env (v1, v2) =
      let v1 = (map_wrap map_exn_clause_kind) env v1 in
      let v2 = map_body_or_clauses env v2 in
      todo env (v1, v2)
    in
    let v2 = (map_list map_tuple2) env v2 in
    todo env (v1, v2)
  in
  (map_bracket map_tuple1) env v

and map_exn_clause_kind env v =
  match v with
  | After -> todo env ()
  | Rescue -> todo env ()
  | Catch -> todo env ()
  | Else -> todo env ()

and map_block env v = (map_bracket map_body_or_clauses) env v

let map_program env v : G.program = map_body env v

let map_any env v =
  match v with
  | Pr v ->
      let v = map_program env v in
      todo env v

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let program x =
  let env = () in
  map_program env x

let any x =
  let env = () in
  map_any env x
