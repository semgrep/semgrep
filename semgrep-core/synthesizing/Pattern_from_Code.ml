(* Emma Jin
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
open AST_generic

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
type named_variants =
  (string * Pattern.t) list

type environment = { count : int; mapping : (expr * expr) list;
                      has_type : bool }

(*****************************************************************************)
(* State helpers *)
(*****************************************************************************)

(* TODO make mapping a map and use map lookup *)
let lookup env e =
let mapping = env.mapping in
let rec look = function
    | [] -> None
    | (e1, e2)::xs ->
      if Matching_generic.equal_ast_binded_code (E e) (E e1) then Some e2 else look xs
in
  look mapping


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let fk = Parse_info.fake_info "fake"
let _bk f (lp,x,rp) = (lp, f x, rp)

let default_id str =
  Id((str, fk),
   {id_resolved = ref None; id_type = ref None; id_const_literal = ref None})

let default_tyvar str typ =
  TypedMetavar((str, fk), fk, typ)

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
let get_id ?(with_type=false) env e =
  let id = lookup env e in
  match id with
      Some x -> (env, x)
    | None ->
        let notype_id = default_id (count_to_id env.count) in
        let has_type = env.has_type in
        let (new_id, new_has_type) =
        if with_type then
          (match e with
              | Id (_, {id_type; _}) ->
                 (match !id_type with
                   | None -> (notype_id, has_type)
                   | Some t -> (default_tyvar (count_to_id env.count) t), true)
              | _ -> (notype_id, has_type)
          )
        else (notype_id, has_type) in
        ({ count = env.count + 1; mapping = (e, new_id)::(env.mapping); has_type = new_has_type }, new_id)

let has_nested_call = List.find_opt (fun x -> match x with Arg(Call _) -> true | _ -> false)

(*****************************************************************************)
(* Algorithm *)
(*****************************************************************************)

(* Calls *)
let shallow_dots (e, (lp, rp)) =
  ("dots", E (Call (e, (lp, [Arg (Ellipsis fk)], rp))))

let rec map_args env = function
  | [] -> (env, [])
  | (Arg x)::xs ->
     let (env', new_id) = get_id env x in
     let (_, args) = map_args env' xs in
     (env', (Arg new_id)::args)
  | (ArgKwd (label, x))::xs ->
    let (env', new_id) = get_id env x in
    let (_, args) = map_args env' xs in
    (env', (ArgKwd (label, new_id))::args)
  | _ -> (env, []) (* TODO fail? *)

let exact_metavar (e, (lp, es, rp)) env =
  let (_, replaced_args) = map_args env es in
  ("exact metavars", E (Call (e, (lp, replaced_args, rp))))

let shallow_metavar (e, (lp, es, rp)) env =
  let (_, replaced_args) = map_args env es in
  let args' = replaced_args @ [Arg (Ellipsis fk)] in
  ("metavars", E (Call (e, (lp, args', rp))))

let rec deep_mv_call (e, (lp, es, rp)) with_type env =
  let (env', replaced_args) = deep_mv_args with_type env es in
  (env', Call (e, (lp, replaced_args, rp)))
and deep_mv_args with_type env args =
  let get_new_arg (e, xs) =
    let (env', new_e) =
      match e with
          | Call (e, (lp, es, rp)) -> deep_mv_call (e, (lp, es, rp)) with_type env
          | _ -> get_id ~with_type:with_type env e
      in
      let (env'', args') = deep_mv_args with_type env' xs in
      (env'', new_e, args')
  in
  match args with
    | [] -> (env, [])
    | (Arg e)::xs -> (
       let (env'', new_e, args') = get_new_arg (e, xs) in
       (env'', (Arg new_e)::args')
    )
    | (ArgKwd (label, e))::xs -> (
       let (env'', new_e, args') = get_new_arg (e, xs) in
       (env'', (ArgKwd (label, new_e))::args')
    )
    | _ -> (env, [])

let deep_metavar (e, (lp, es, rp)) env =
  let (_, e') = deep_mv_call (e, (lp, es, rp)) false env in ("deep metavars", E e')

let deep_typed_metavar (e, (lp, es, rp)) env =
  let (env', e') = deep_mv_call (e, (lp, es, rp)) true env in
  if env'.has_type then Some ("typed metavars", E e') else None


let generalize_call env = function
  | Call (IdSpecial (New, _), _) -> []
  | Call (e, (lp, es, rp)) ->
      (* only show the deep_metavar and deep_typed_metavar options if relevant *)
      let d_mvar =
        match (has_nested_call es) with
            None -> []
          | Some _ -> (deep_metavar (e, (lp, es, rp)) env) :: []
      in
      let optional =
        match (deep_typed_metavar (e, (lp, es, rp)) env) with
            None -> d_mvar
          | Some e' -> e' :: d_mvar
      in
      (match e with
        | IdSpecial _ -> (exact_metavar (e, (lp, es, rp)) env) :: optional
        | _ -> (shallow_dots (e, (lp, rp))) :: (shallow_metavar (e, (lp, es, rp)) env) ::
               (exact_metavar (e, (lp, es, rp)) env) :: optional)
  | _ -> []

(* Id *)
let generalize_id env e =
  match e with
  | Id _ ->
     let (_, id) = get_id env e in
     let (env', id_t) = get_id ~with_type:true env e in
     if env'.has_type then ["metavar", E id; "typed metavar", E id_t]
     else ["metavar", E id]
  | _ -> []

(* Assign *)
let rec include_e2_patterns env (e1, tok, e2) =
  let (env', id) = get_id env e1 in
  let e2_patterns = generalize_exp e2 env' in
  List.map (fun x -> match x with | (s, E pat) -> ("righthand " ^ s, E (Assign(id, tok, pat)))
                                  | _ -> raise (UnexpectedCase "Must pass in an any of form E x"))
           e2_patterns

and generalize_assign env e =
  match e with
    | Assign (e1, tok, e2) ->
      let (env1, id1) = get_id env e1 in
      let (_, id2) = get_id env1 e2 in
      let (env', id_t) = get_id ~with_type:true env e1 in
      let e2_patterns = include_e2_patterns env (e1, tok, e2) in
      let metavar_part =
        if env'.has_type then ["metavars", E (Assign (id1, tok, id2));
                               "typed metavars", E (Assign (id_t, tok, id2))]
        else ["metavars", E (Assign (id1, tok, id2))]
      in
        ("dots", E (Assign (e1, tok, Ellipsis fk)))::(metavar_part @ e2_patterns)
    | _ -> []

(* All expressions *)
and generalize_exp e env =
  match e with
  | Call _ -> generalize_call env e
  | Id _ -> generalize_id env e
  | L _ -> let (_, id) = get_id env e in ["metavar", E id]
  | DotAccess _ -> let (_, id) = get_id env e in ["metavar", E id]
  | Assign _ -> generalize_assign env e
  | _ -> []

(* All statements *)
and generalize_exprstmt (e, tok) env =
  List.map (fun x -> match x with | (s, E e') -> (s, S (ExprStmt(e', tok)))
                                  | _ -> raise (UnexpectedCase "Must pass in an any of form E x"))
           (generalize_exp e env)

and generalize_if s_in =
  let opt f so =
    match so with
      | None -> None
      | Some x -> Some (f x)
  in
  let rec dots_in_body s =
    match s with
      | If (tok, e, s, sopt) -> If (tok, e, dots_in_body s, opt dots_in_body sopt)
      | Block (t1, _, t2) -> Block(t1, [ExprStmt (Ellipsis fk, fk)], t2)
      | _ -> ExprStmt (Ellipsis fk, fk)
  in
  let rec dots_in_cond s =
    match s with
      | If (tok, _, s, sopt) -> If (tok, Ellipsis fk, s, opt dots_in_cond sopt)
      | Block (t1, [If _ as x], t2) -> Block(t1, [dots_in_cond x], t2)
      | x -> x
  in
  ["dots in body", S (dots_in_body s_in); "dots in cond", S (dots_in_cond s_in)]

and generalize_block ss =
  let rec get_last = function
  | [] -> []
  | [x] -> [x]
  | _::xs -> get_last xs
  in
  match ss with
  | [] | _::[] -> ss
  | x::_::[] -> x::(ExprStmt(Ellipsis fk, fk))::[]
  | x::y::z::zs -> x::(ExprStmt(Ellipsis fk, fk))::(get_last (y::z::zs))

and generalize_stmt s env =
  match s with
  | ExprStmt (e, tok) -> generalize_exprstmt (e, tok) env
  | If _ -> generalize_if s
  | Block (t1, ss, t2) -> ["dots", S (Block ((t1, generalize_block ss, t2)))]
  | _ -> []

(* All *)
and generalize_any a env =
   match a with
   | E e -> generalize_exp e env
   | S s -> generalize_stmt s env
   | _ -> []

let generalize a =
  (generalize_any a { count = 1; mapping = []; has_type = false })

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let from_any a =
  ("exact match", a)::
  generalize a
