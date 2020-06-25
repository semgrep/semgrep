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

type global_state = { count : int; mapping : (expr * expr) list }

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

let count_to_id count =
  let make_id ch = Format.sprintf "$%c" ch in
  if count = 1 then make_id 'X'
  else if count = 2 then make_id 'Y'
  else if count = 3 then make_id 'Z'
  else if count <= 26 then make_id (Char.chr (count - 4 + Char.code 'A'))
  else Format.sprintf "$X%d" (count - 26)

let get_id state e =
  let id = lookup state e in
  match id with
      Some x -> (state, x)
    | None ->
        let new_id = default_id (count_to_id state.count) in
        ({ count = state.count + 1; mapping = (e, new_id)::(state.mapping) }, new_id)

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
     let (c, new_id) = get_id state x in
     let (_, args) = map_args c xs in
     (c, (Arg new_id)::args)
  | _ -> (state, []) (* TODO fail? *)

let shallow_metavar (e, (lp, es, rp)) state =
  let (_, replaced_args) = map_args state es in
  ("metavars", E (Call (e, (lp, replaced_args, rp))))

let generalize_call state = function
  | Call (IdSpecial e1, e2) -> (shallow_metavar (IdSpecial e1, e2) state) :: []
  | Call (e, (lp, es, rp)) ->
      (shallow_dots (e, (lp, rp))) :: (shallow_metavar (e, (lp, es, rp)) state) :: []
  | _ -> []

(* All expressions *)
let generalize_exp e state =
  match e with
  | Call _ -> generalize_call state e
  | Id _ -> let (_, id) = get_id state e in ["metavar", E id]
  | L _ -> let (_, id) = get_id state e in ["metavar", E id]
  | _ -> []

let generalize e =
  generalize_exp e { count = 1; mapping = [] }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let from_expr e =
  ("exact match", E e)::
  generalize e
