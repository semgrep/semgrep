(* Brandon Wu
 *
 * Copyright (c) 2023 Semgrep Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* See the .mli for why this is important.
 *
 * In Javascript systems, we choose a 64 implementation for integers, which is
 * more in line with the native integers used by the Javascript runtime.
 * This reduces inconsistency in the JS translation.
 *)

open Sexplib.Conv

(*****************************************************************************)
(* Int64 boilerplate *)
(*****************************************************************************)

(* There isn't an available hash_fold or hash function in Int64.t, so we have to
   make it up here.
   Unfortunately, there doesn't seem to be a direct hash function for int64 in
   the Ppx_hash_lib either, so we just write what it would be.
*)
let hash_fold_int64 = Ppx_hash_lib.Std.Hash.fold_int64

let hash_int64 x =
  Ppx_hash_lib.Std.Hash.(get_hash_value (fold_int64 (reset (alloc ())) x))

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* alt: int64 option? *)
(* alt: int64 option wrap? *)
(* could save consumers of the API from dealing with whether the concrete int
   is representable or not
*)
type t = int64 [@@deriving hash, show, ord, eq, sexp]

(*****************************************************************************)
(* Creators *)
(*****************************************************************************)

let of_float = Int64.of_float
let of_int x = Int64.of_int x
let of_int64 x = x
let of_string_opt = Common2.int64_of_string_opt
let of_string_c_octal_opt = Common2.int64_of_string_c_octal_opt
let zero = 0L
let neg = Int64.neg

(*****************************************************************************)
(* Destructors *)
(*****************************************************************************)

let to_int64 x = x
let to_int = Int64.to_int
let to_v = OCaml.vof_int64
let to_string = Int64.to_string
let to_float = Int64.to_float

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let eq_const i1 i2 = Int64.equal i1 (Int64.of_int i2)
