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
 * On Unix systems, we choose a standard 31/63-bit implementation for integers,
 * which have advantageous properties when it comes to performance, for garbage
 * collection reasons.
 * https://blog.janestreet.com/what-is-gained-and-lost-with-63-bit-integers/
 *)
open Ppx_sexp_conv_lib.Conv

(*****************************************************************************)
(* Int boilerplate *)
(*****************************************************************************)

(* There isn't an available hash_fold or hash function in Int64.t, so we have to
   make it up here.
   Unfortunately, there doesn't seem to be a direct hash function for int64 in
   the Ppx_hash_lib either, so we just write what it would be.
*)
let hash_fold_int = Ppx_hash_lib.Std.Hash.fold_int

let hash_int x =
  Ppx_hash_lib.Std.Hash.(get_hash_value (fold_int (reset (alloc ())) x))

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* alt: int option? *)
(* alt: int option wrap? *)
(* could save consumers of the API from dealing with whether the concrete int
   is representable or not
*)
type t = int [@@deriving hash, show, ord, eq, sexp]

(*****************************************************************************)
(* Creators *)
(*****************************************************************************)

let of_float = Int.of_float
let of_int x = x
let of_int64 x = Int64.to_int x
let of_string_opt = Common2.int_of_string_opt
let of_string_c_octal_opt = Common2.int_of_string_c_octal_opt
let zero = 0
let neg = Int.neg

(*****************************************************************************)
(* Destructors *)
(*****************************************************************************)

let to_int64 x = Int64.of_int x
let to_int x = x
let to_v = OCaml.vof_int
let to_string = Int.to_string
let to_float = Int.to_float

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let eq_const i1 i2 = Int.equal i1 i2
