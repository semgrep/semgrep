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

type global_state = { count : int; mapping : (expr * expr) list;
                      has_type : bool }

(*****************************************************************************)
(* State helpers *)
(*****************************************************************************)

(* TODO make mapping a map and use map lookup *)
let lookup state e =
let mapping = state.mapping in
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
  if count = 1 then make_id 'X'
  else if count = 2 then make_id 'Y'
  else if count = 3 then make_id 'Z'
  else if count <= 26 then make_id (Char.chr (count - 4 + Char.code 'A'))
  else Format.sprintf "$X%d" (count - 26)

(* If the id is already in state, return that *)
(* Otherwise, this depends on the with_type flag *)
(* If with_type is true, if there is a type, try to generate a TypedMetavar *)
(* In all other cases, generate an id *)
(* Add to state's mapping and return it *)
let get_id ?(with_type=false) state e =
  let id = lookup state e in
  match id with
      Some x -> (state, x)
    | None ->
        let notype_id = default_id (count_to_id state.count) in
        let has_type = state.has_type in
        let (new_id, new_has_type) =
        if with_type then
          ( match e with
              | Id (_, {id_type; _}) ->
                 (match !id_type with | None -> (notype_id, has_type)
                                      | Some t -> (default_tyvar (count_to_id state.count) t), true)
              | _ -> (notype_id, has_type)
          )
        else (notype_id, has_type) in
        ({ count = state.count + 1; mapping = (e, new_id)::(state.mapping); has_type = new_has_type }, new_id)

let has_nested_call = List.find_opt (fun x -> match x with Arg(Call _) -> true | _ -> false)

(*****************************************************************************)
(* Algorithm *)
(*****************************************************************************)
(* TODO: look if Call, and propose $ on each argument, ...,
 * propose also typed metavars, and resolved id in the call, etc.
 *)

(* Calls *)
let shallow_dots (e, (lp, rp)) =
  ("dots", E (Call (e, (lp, [Arg (Ellipsis fk)], rp))))

let rec map_args state = function
  | [] -> (state, [])
  | (Arg x)::xs ->
     let (state', new_id) = get_id state x in
     let (_, args) = map_args state' xs in
     (state', (Arg new_id)::args)
  | _ -> (state, []) (* TODO fail? *)

let shallow_metavar (e, (lp, es, rp)) state =
  let (_, replaced_args) = map_args state es in
  ("metavars", E (Call (e, (lp, replaced_args, rp))))

let rec deep_mv_call (e, (lp, es, rp)) with_type state =
  let (state', replaced_args) = deep_mv_args with_type state es in
  (state', Call (e, (lp, replaced_args, rp)))
and deep_mv_args with_type state = function
  | [] -> (state, [])
  | (Arg e)::xs -> (
     let (state', new_e) =
     match e with
         | Call (e, (lp, es, rp)) -> deep_mv_call (e, (lp, es, rp)) with_type state
         | _ -> get_id ~with_type:with_type state e
     in
     let (_, args) = deep_mv_args with_type state' xs in
     (state', (Arg new_e)::args)
  )
  | _ -> (state, [])

let deep_metavar (e, (lp, es, rp)) state =
  let (_, e') = deep_mv_call (e, (lp, es, rp)) false state in ("deep metavars", E e')

let deep_typed_metavar (e, (lp, es, rp)) state =
  let (state', e') = deep_mv_call (e, (lp, es, rp)) true state
  in
  if state'.has_type then Some ("typed metavars", E e') else None


let generalize_call state = function
  | Call (IdSpecial e1, e2) -> (shallow_metavar (IdSpecial e1, e2) state) :: []
  | Call (e, (lp, es, rp)) ->
      (* only show the deep_metavar and deep_typed_metavar options if relevant *)
      let d_mvar =
        match (has_nested_call es) with
            None -> []
          | Some _ -> (deep_metavar (e, (lp, es, rp)) state) :: []
      in
      let optional =
        match (deep_typed_metavar (e, (lp, es, rp)) state) with
            None -> d_mvar
          | Some e' -> e' :: d_mvar
      in
       (shallow_dots (e, (lp, rp))) :: (shallow_metavar (e, (lp, es, rp)) state) :: optional
  | _ -> []

(* All expressions *)
let generalize_exp e state =
  match e with
  | Call _ -> generalize_call state e
  | Id _ -> let (_, id) = get_id state e in ["metavar", E id]
  | L _ -> let (_, id) = get_id state e in ["metavar", E id]
  | _ -> []

let generalize e =
  (generalize_exp e { count = 1; mapping = []; has_type = false })

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let from_expr e =
  ("exact match", E e)::
  generalize e
