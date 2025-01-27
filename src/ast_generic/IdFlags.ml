(* Copyright (C) 2023-present Semgrep Inc.
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

open Ppx_hash_lib.Std.Hash.Builtin

let bitmask_HIDDEN = 0x01
let bitmask_CASE_INSENSITIVE = 0x02
let bitmask_FINAL = 0x04
let bitmask_STATIC = 0x08

type t = int [@@deriving show, eq, ord, hash]

let empty = 0
let get_flag bitmask flags = Int.logand flags bitmask <> 0
let set_flag bitmask flags = Int.logor flags bitmask
let make_flag bitmask = (get_flag bitmask, set_flag bitmask)
let is_hidden, set_hidden = make_flag bitmask_HIDDEN

let is_case_insensitive, set_case_insensitive =
  make_flag bitmask_CASE_INSENSITIVE

let is_final, set_final = make_flag bitmask_FINAL
let is_static, set_static = make_flag bitmask_STATIC
let union x y = Int.logor x y
let to_int x = x

let make ~hidden ~case_insensitive ~final ~static =
  let flags = empty in
  let flags = if hidden then set_hidden flags else flags in
  let flags = if case_insensitive then set_case_insensitive flags else flags in
  let flags = if final then set_final flags else flags in
  let flags = if static then set_static flags else flags in
  flags
