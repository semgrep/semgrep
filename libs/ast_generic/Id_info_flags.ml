(* Andre Kuhlenschmidt
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

(* The following library implements a packed representation of a
 * concrete set of flags. It is designed to use no more space than the
 * ocaml int type so that it doesn't have to be allocated. This
 * library essentially allows you to add somewhere between 31-63
 * boolean flags to ids without affecting memory layout of the id_info
 * type. *)

module IB = Immediate_bitfield

type packed = IB.t
type unpacked = { hidden : bool; case_insensitive : bool }
type packed_field = int

(* Boolean fields are offsets into our immediate bitfield. *)
let hidden_field : packed_field = 0
let case_insensitive_field : packed_field = 1

let pack flags =
  let packed = IB.empty in
  let packed = IB.set_bit packed hidden_field flags.hidden in
  let packed =
    IB.set_bit packed case_insensitive_field flags.case_insensitive
  in
  packed

(* Adding individual getters because it is more often the case that we will just care about getting one particular flag as oposed to setting one flag a time. *)
let get field flags = IB.get_bit flags field
let is_hidden = get hidden_field
let is_case_insensitive = get case_insensitive_field

let unpack flags =
  { hidden = is_hidden flags; case_insensitive = is_case_insensitive flags }

let snoc ls a = List.rev ls |> fun x -> a :: x |> List.rev

let show_unpacked flags =
  (* Written assuming this list is usually going to be small or empty. *)
  let ls = [] in
  (* Flag strings conditionally consed on to list in alphabetical order. *)
  let ls = if flags.hidden then "hidden" :: ls else ls in
  let ls = if flags.case_insensitive then "case_insensitive" :: ls else ls in
  (* Reversing the list puts the list itself in alphabetical order. *)
  let alphabetical_ls = List.rev ls in
  (* Add seperators between exisiting elements. *)
  let sep_ls = Base.List.intersperse ~sep:"; " alphabetical_ls in
  (* Add curlies on the outside *)
  let curlies_sep_ls = "{" :: snoc sep_ls "}" in
  Base.String.concat curlies_sep_ls

let show_packed flags = unpack flags |> show_unpacked
let equal_packed : packed -> packed -> bool = IB.equal
let equal_unpacked f1 f2 = equal_packed (pack f1) (pack f2)
let hash_packed = IB.hash
let hash_fold_packed = IB.hash_fold_t

(* TODO: I am not sure how this should be implemented. I think
 * something in ppx_deriving show for AST_generic requires it. *)
let pp_packed _fmt _flags = ()
