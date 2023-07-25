(* Andre Kuhlenschmidt
 *
 * Copyright (C) 2023 r2c
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

open Base.Int

type t = int

let empty = 0
let num_bits = Base.Int.num_bits
let max_bit = num_bits - 1
let min_bit = 0

exception Index_exn of int

let get_bit t b =
  if min_bit > b || b > max_bit then raise (Index_exn b);
  let mask = shift_left 1 b in
  let masked = bit_and t mask in
  shift_right masked b == 1

let set_bit t b v =
  if min_bit > b || b > max_bit then raise (Index_exn b);
  (* Clear the bit unconditionally, then set the bit if needed *)
  let mask = bit_not (shift_left 1 b) in
  let t_cleared_bit = bit_and mask t in
  let shifted_cond_bit = shift_left (if v then 1 else 0) b in
  bit_xor t_cleared_bit shifted_cond_bit

let show t =
  let show_bit b = if get_bit t b then '1' else '0' in
  Base.String.init num_bits ~f:show_bit

let equal = Common.( =|= )
let hash = Base.Int.hash
let hash_fold_t = Base.Hash.fold_int
