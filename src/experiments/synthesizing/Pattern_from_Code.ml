(* Emma Jin
 *
 * Copyright (C) 2020 r2c
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
open AST_generic
module G = AST_generic
module H = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The main pattern-from-code synthesizing algorithm.
 *
 * related work:
 *  - coccinelle spinfer?
 *)

exception UnexpectedCase of string

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type named_variants = (string * Pattern.t) list

type environment = {
  config : Rule_options.t;
  count : int;
  mapping : (expr * expr) list;
  has_type : bool;
}

(*****************************************************************************)
(* State helpers *)
(*****************************************************************************)

(* TODO make mapping a map and use map lookup *)
let lookup env e =
  let mapping = env.mapping in
  let rec look = function
    | [] -> None
    | (e1, e2) :: xs ->
        if Matching_generic.equal_ast_bound_code env.config (E e) (E e1) then
          Some e2
        else look xs
  in
  look mapping

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fk = Tok.unsafe_fake_tok "fake"
let fk_stmt = ExprStmt (Ellipsis fk |> G.e, fk) |> G.s
let body_ellipsis t1 t2 = Block (t1, [ fk_stmt ], t2) |> G.s
let _bk f (lp, x, rp) = (lp, f x, rp)

let default_id str =
  G.N (Id ((str, fk), empty_id_info ~id:IdInfoId.unsafe_default ())) |> G.e

let default_tyvar str typ = TypedMetavar ((str, fk), fk, typ) |> G.e

let count_to_id count =
  let make_id ch = Format.sprintf "$%c" ch in
  match count with
  | 1 -> make_id 'X'
  | 2 -> make_id 'Y'
  | 3 -> make_id 'Z'
  | _ when count <= 26 -> make_id (Char.chr (count - 4 + Char.code 'A'))
  | _ -> Format.sprintf "$X%d" (count - 26)

(* If the id is already in env, return that *)
(* Otherwise, this depends on the with_type flag *)
(* If with_type is true, if there is a type, try to generate a TypedMetavar *)
(* In all other cases, generate an id *)
(* Add to env's mapping and return it *)
let get_id ?(with_type = false) env e =
  let id = lookup env e in
  match id with
  | Some x -> (env, x)
  | None ->
      let notype_id = default_id (count_to_id env.count) in
      let has_type = env.has_type in
      let new_id, new_has_type =
        if with_type then
          match e.e with
          | N (Id (_, { id_type; _ })) -> (
              match !id_type with
              | None -> (notype_id, has_type)
              | Some t -> (default_tyvar (count_to_id env.count) t, true))
          | L (String (l, (_, tag), r)) ->
              (L (String (l, ("...", tag), r)) |> G.e, false)
          | _ -> (notype_id, has_type)
        else
          match e.e with
          | L (String (l, (_, tag), r)) ->
              (L (String (l, ("...", tag), r)) |> G.e, false)
          | _ -> (notype_id, has_type)
      in
      let env' =
        {
          env with
          count = env.count + 1;
          mapping = (e, new_id) :: env.mapping;
          has_type = new_has_type;
        }
      in
      (env', new_id)

let has_nested_call =
  List.find_opt (fun x ->
      match x with
      | Arg { e = Call _; _ } -> true
      | _ -> false)

(*****************************************************************************)
(* Algorithm *)
(*****************************************************************************)

(* Calls *)
let shallow_dots (e, (lp, rp)) =
  ("dots", E (Call (e, (lp, [ Arg (Ellipsis fk |> G.e) ], rp)) |> G.e))

let rec map_args env = function
  | [] -> (env, [])
  | Arg x :: xs ->
      let env', new_id = get_id env x in
      let _, args = map_args env' xs in
      (env', Arg new_id :: args)
  | ArgKwd (label, x) :: xs ->
      let env', new_id = get_id env x in
      let _, args = map_args env' xs in
      (env', ArgKwd (label, new_id) :: args)
  | _ -> (env, [])

(* TODO fail? *)

let exact_metavar (e, (lp, es, rp)) env =
  let _, replaced_args = map_args env es in
  ("exact metavars", E (Call (e, (lp, replaced_args, rp)) |> G.e))

let shallow_metavar (e, (lp, es, rp)) env =
  let _, replaced_args = map_args env es in
  let args' = replaced_args @ [ Arg (Ellipsis fk |> G.e) ] in
  ("metavars", E (Call (e, (lp, args', rp)) |> G.e))

let rec deep_mv_call (e, (lp, es, rp)) with_type env =
  let env', replaced_args = deep_mv_args with_type env es in
  (env', Call (e, (lp, replaced_args, rp)) |> G.e)

and deep_mv_args with_type env args =
  let get_new_arg (e, xs) =
    let env', new_e =
      match e.e with
      | Call (e, (lp, es, rp)) -> deep_mv_call (e, (lp, es, rp)) with_type env
      | _ -> get_id ~with_type env e
    in
    let env'', args' = deep_mv_args with_type env' xs in
    (env'', new_e, args')
  in
  match args with
  | [] -> (env, [])
  | Arg e :: xs ->
      let env'', new_e, args' = get_new_arg (e, xs) in
      (env'', Arg new_e :: args')
  | ArgKwd (label, e) :: xs ->
      let env'', new_e, args' = get_new_arg (e, xs) in
      (env'', ArgKwd (label, new_e) :: args')
  | _ -> (env, [])

let deep_metavar (e, (lp, es, rp)) env =
  let _, e' = deep_mv_call (e, (lp, es, rp)) false env in
  ("deep metavars", E e')

let deep_typed_metavar (e, (lp, es, rp)) env =
  let env', e' = deep_mv_call (e, (lp, es, rp)) true env in
  if env'.has_type then Some ("typed metavars", E e') else None

let generalize_call env e =
  match e.e with
  | New (_, _, _, _) -> []
  | Call (e, (lp, es, rp)) -> (
      (* only show the deep_metavar and deep_typed_metavar options if relevant *)
      let d_mvar =
        match has_nested_call es with
        | None -> []
        | Some _ -> [ deep_metavar (e, (lp, es, rp)) env ]
      in
      let optional =
        match deep_typed_metavar (e, (lp, es, rp)) env with
        | None -> d_mvar
        | Some e' -> e' :: d_mvar
      in
      match e.e with
      | IdSpecial _ -> exact_metavar (e, (lp, es, rp)) env :: optional
      | _ ->
          shallow_dots (e, (lp, rp))
          :: shallow_metavar (e, (lp, es, rp)) env
          :: exact_metavar (e, (lp, es, rp)) env
          :: optional)
  | _ -> []

(* Id *)
let generalize_id env e =
  match e.e with
  | G.N (Id _) ->
      let _, id = get_id env e in
      let env', id_t = get_id ~with_type:true env e in
      if env'.has_type then [ ("metavar", E id); ("typed metavar", E id_t) ]
      else [ ("metavar", E id) ]
  | _ -> []

(* Assign *)
let rec include_e2_patterns env (e1, tok, e2) =
  let env', id = get_id env e1 in
  add_expr e2
    (fun (s, pat) -> ("righthand " ^ s, E (Assign (id, tok, pat) |> G.e)))
    env'

and generalize_assign env e =
  match e.e with
  | Assign (e1, tok, e2) ->
      let env1, id1 = get_id env e1 in
      let _, id2 = get_id env1 e2 in
      let env', id_t = get_id ~with_type:true env e1 in
      let e2_patterns = include_e2_patterns env (e1, tok, e2) in
      let metavar_part =
        if env'.has_type then
          [
            ("metavars", E (Assign (id1, tok, id2) |> G.e));
            ("typed metavars", E (Assign (id_t, tok, id2) |> G.e));
          ]
        else [ ("metavars", E (Assign (id1, tok, id2) |> G.e)) ]
      in
      ("dots", E (Assign (e1, tok, Ellipsis fk |> G.e) |> G.e))
      :: (metavar_part @ e2_patterns)
  | _ -> []

(* All expressions *)
and generalize_exp e env =
  match e.e with
  | Call _ -> generalize_call env e
  | N (Id _) -> generalize_id env e
  | L _ ->
      let _, id = get_id env e in
      [ ("metavar", E id) ]
  | DotAccess _ ->
      let _, id = get_id env e in
      [ ("metavar", E id) ]
  | Assign _ -> generalize_assign env e
  | _ -> []

(* Helper functions to make it easier to add all variations *)
(* Generalizes e, then applies the same transformation f to each *)
and add_expr e f env =
  List_.map
    (fun x ->
      match x with
      | str, E e' -> f (str, e')
      | _ -> raise (UnexpectedCase "Must pass in an any of form E x"))
    (generalize_exp e env)

and add_stmt s f env =
  List_.map
    (fun x ->
      match x with
      | str, S s' -> f (str, s')
      | _ -> raise (UnexpectedCase "Must pass in an any of form S x"))
    (generalize_stmt s env)

(* All statements *)
and generalize_exprstmt (e, tok) env =
  add_expr e (fun (str, e') -> (str, S (ExprStmt (e', tok) |> G.s))) env

and generalize_if s_in =
  let opt f so =
    match so with
    | None -> None
    | Some x -> Some (f x)
  in
  let rec dots_in_body s =
    match s.s with
    | If (tok, e, s, sopt) ->
        If (tok, e, dots_in_body s, opt dots_in_body sopt) |> G.s
    | Block (t1, [ ({ s = If _; _ } as x) ], t2) ->
        Block (t1, [ dots_in_body x ], t2) |> G.s
    | Block (t1, _, t2) -> body_ellipsis t1 t2
    | _ -> fk_stmt
  in
  let rec dots_in_cond s =
    match s.s with
    | If (tok, _, s, sopt) ->
        If (tok, Cond (Ellipsis fk |> G.e), s, opt dots_in_cond sopt) |> G.s
    | Block (t1, [ ({ s = If _; _ } as x) ], t2) ->
        Block (t1, [ dots_in_cond x ], t2) |> G.s
    | _ -> s
  in
  [
    ("dots in body", S (dots_in_body s_in));
    ("dots in cond", S (dots_in_cond s_in));
  ]

and generalize_while (tok, cond, s) env =
  let body_dots =
    match s.s with
    | Block (t1, _, t2) -> body_ellipsis t1 t2
    | _ -> fk_stmt
  in
  let dots_in_cond =
    ("dots in condition", S (While (tok, G.Cond (Ellipsis fk |> G.e), s) |> G.s))
  in
  let dots_in_body =
    ("dots in body", S (While (tok, cond, body_dots) |> G.s))
  in
  let expr_choices_in_cond =
    add_expr (H.cond_to_expr cond)
      (fun (str, e') ->
        ("condition " ^ str, S (While (tok, G.Cond e', body_dots) |> G.s)))
      env
  in
  dots_in_cond :: dots_in_body :: expr_choices_in_cond

and generalize_for (tok, hdr, s) =
  let body_dots =
    match s.s with
    | Block (t1, _, t2) -> body_ellipsis t1 t2
    | _ -> fk_stmt
  in
  let dots_in_body = ("dots in body", S (For (tok, hdr, body_dots) |> G.s)) in
  let dots_in_cond =
    ("dots in condition", S (For (tok, ForEllipsis fk, s) |> G.s))
  in
  [ dots_in_cond; dots_in_body ]

and generalize_block (t1, ss, t2) env =
  let rec get_last = function
    | [] -> []
    | [ x ] -> [ x ]
    | _ :: xs -> get_last xs
  in
  match ss with
  | [] -> []
  | [ x ] ->
      add_stmt x
        (fun (str, s') -> ("with " ^ str, S (Block (t1, [ s' ], t2) |> G.s)))
        env
  | [ x; _ ] -> [ ("dots", S (Block (t1, [ x; fk_stmt ], t2) |> G.s)) ]
  | x :: y :: z :: zs ->
      [
        ( "dots",
          S (Block (t1, x :: fk_stmt :: get_last (y :: z :: zs), t2) |> G.s) );
      ]

(* All statements *)
and generalize_stmt s env =
  match s.s with
  | ExprStmt (e, tok) -> generalize_exprstmt (e, tok) env
  | If _ -> generalize_if s
  | While (tok, e, s) -> generalize_while (tok, e, s) env
  | For (tok, hdr, s) -> generalize_for (tok, hdr, s)
  | Block (t1, ss, t2) -> generalize_block (t1, ss, t2) env
  | _ -> []

(* All *)
and generalize_any a env =
  match a with
  | E e -> generalize_exp e env
  | S s -> generalize_stmt s env
  | _ -> []

let generalize config a =
  generalize_any a { config; count = 1; mapping = []; has_type = false }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let from_any config a = ("exact match", a) :: generalize config a
