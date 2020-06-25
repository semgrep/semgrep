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

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let fk = Parse_info.fake_info "fake"
let _bk f (lp,x,rp) = (lp, f x, rp)

(* let fst (x, _) = x *)
let snd (_, x) = x

(*****************************************************************************)
(* Algorithm *)
(*****************************************************************************)
(* TODO: look if Call, and propose $ on each argument, ...,
 * propose also typed metavars, and resolved id in the call, etc.
 *)

let default_id count =
    (count + 1,
     Id((Format.sprintf "$X%d" (count), Parse_info.fake_info " "),
     {id_resolved = ref None; id_type = ref None; id_const_literal = ref None})
    )

let shallow_dots (e, (lp, rp)) =
   ("dots", E (Call (e, (lp, [Arg (Ellipsis fk)], rp))))

let rec map_args count = function
   | [] -> (count, [])
   | _x::xs ->
      let (c, new_id) = default_id count in
      let (_, args) = map_args c xs in
      (c, (Arg new_id)::args)

let shallow_metavar (e, (lp, es, rp)) count =
   let (_, replaced_args) = map_args count es in
   ("metavars", E (Call (e, (lp, replaced_args, rp))))


let generalize_call count = function
  | Call (IdSpecial e1, e2) -> (shallow_metavar (IdSpecial e1, e2) count) :: []
  | Call (e, (lp, es, rp)) ->
      (shallow_dots (e, (lp, rp))) :: (shallow_metavar (e, (lp, es, rp)) count) :: []
  | _ -> []

let generalize_exp e count =
  match e with
  | Call _ -> generalize_call count e
  | Id ((_s, t1), info) ->
      ["metavar", E (Id(("$X", t1), info))]
  | L _ -> ["metavar", E (snd (default_id count))]
  | _ -> []

let generalize e =
  let count = 1 in
  generalize_exp e count

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let from_expr e =
  ("exact match", E e)::
  generalize e
