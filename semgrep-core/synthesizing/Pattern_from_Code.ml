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
and deep_mv_args with_type env = function
  | [] -> (env, [])
  | (Arg e)::xs -> (
     let (env', new_e) =
     match e with
         | Call (e, (lp, es, rp)) -> deep_mv_call (e, (lp, es, rp)) with_type env
         | _ -> get_id ~with_type:with_type env e
     in
     let (env'', args) = deep_mv_args with_type env' xs in

     (env'', (Arg new_e)::args)
  )
  | _ -> (env, [])

let deep_metavar (e, (lp, es, rp)) env =
  let (_, e') = deep_mv_call (e, (lp, es, rp)) false env in ("deep metavars", E e')

let deep_typed_metavar (e, (lp, es, rp)) env =
  let (env', e') = deep_mv_call (e, (lp, es, rp)) true env in
  if env'.has_type then Some ("typed metavars", E e') else None


let generalize_call env = function
  | Call (IdSpecial e1, e2) -> (exact_metavar (IdSpecial e1, e2) env) :: []
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
       (shallow_dots (e, (lp, rp))) :: (shallow_metavar (e, (lp, es, rp)) env) ::
       (exact_metavar (e, (lp, es, rp)) env) :: optional
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

(* All expressions *)
let generalize_exp e env =
  match e with
  | Call _ -> generalize_call env e
  | Id _ -> generalize_id env e
  | L _ -> let (_, id) = get_id env e in ["metavar", E id]
  | DotAccess _ -> let (_, id) = get_id env e in ["metavar", E id]
  | _ -> []

let generalize e =
  (generalize_exp e { count = 1; mapping = []; has_type = false })

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let from_expr e =
  ("exact match", E e)::
  generalize e
