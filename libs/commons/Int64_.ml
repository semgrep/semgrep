(*
   Copyright (c) 2023-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
include Int64
open Ppx_hash_lib.Std.Hash.Builtin
open Sexplib.Std

(* There isn't an available hash_fold or hash function in Int64.t, so we have to
   make it up here.
   Unfortunately, there doesn't seem to be a direct hash function for int64 in
   the Ppx_hash_lib either, so we just write what it would be.
*)
let hash_fold_int64 = Ppx_hash_lib.Std.Hash.fold_int64

type t = int64 [@@deriving hash, show, eq, sexp]

let rec power x n =
  if equal n 0L then 1L
  else if equal (Int64.rem n 2L) 0L then
    let y = power x (Int64.div n 2L) in
    mul y y
  else mul x (power x (Int64.sub n 1L))

let ( + ) = Int64.add
let ( - ) = Int64.sub
let ( * ) = Int64.mul
let ( / ) = Int64.div
let ( mod ) = Int64.rem
let ( asr ) = Int64.shift_right
let ( lsl ) = Int64.shift_left
let ( lsr ) = Int64.shift_right_logical
let ( lor ) = Int64.logor
let ( land ) = Int64.logand
let ( lxor ) = Int64.logxor
let ( =|= ) = Int64.equal
let ( > ) i1 i2 = Int64.compare i1 i2 > 0
let ( < ) i1 i2 = Int64.compare i1 i2 < 0
let ( >= ) i1 i2 = Int64.compare i1 i2 >= 0
let ( <= ) i1 i2 = Int64.compare i1 i2 <= 0
