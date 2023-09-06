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

type t = int [@@deriving show, eq, ord, hash]

let empty = 0
let is_hidden flags = Int.logand flags bitmask_HIDDEN <> 0
let set_hidden flags = Int.logor flags bitmask_HIDDEN
let is_case_insensitive flags = Int.logand flags bitmask_CASE_INSENSITIVE <> 0
let set_case_insensitive flags = Int.logor flags bitmask_CASE_INSENSITIVE
let to_int x = x

let make ~hidden ~case_insensitive =
  let flags = empty in
  let flags = if hidden then set_hidden flags else flags in
  let flags = if case_insensitive then set_case_insensitive flags else flags in
  flags
