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

open Ppx_hash_lib.Std.Hash.Builtin
open Sexplib.Std

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* alt: int64 option? *)
(* alt: int64 option wrap? *)
(* could save consumers of the API from dealing with whether the parsed int
   is representable or not
*)
type t = Int64_.t option * Tok.t_always_equal
[@@deriving hash, show, eq, ord, sexp]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let promote opt = (opt, Tok.unsafe_fake_tok "")

let map f (opt, t) =
  match opt with
  | None -> (opt, t)
  | Some i64 -> (Some (f i64), t)

(*****************************************************************************)
(* Creators *)
(*****************************************************************************)

let parse (s, t) = (Common2.int64_of_string_opt s, t)
let parse_c_octal (s, t) = (Common2.int64_of_string_c_octal_opt s, t)

let of_float f =
  let iopt =
    try Some (Int64.of_float f) with
    | _ -> None
  in
  iopt |> promote

let of_int i = Some (Int64.of_int i) |> promote
let of_int64 i64 = Some i64 |> promote

let of_string_opt s =
  match Common2.int64_of_string_opt s with
  | None -> None
  | Some i64 -> Some (Some i64 |> promote)

let fake_zero = Some 0L |> promote
let neg = map Int64.neg
let map_tok f (opt, t) = (opt, f t)

(*****************************************************************************)
(* Destructors *)
(*****************************************************************************)

let to_int_opt (opt, _) = Option.map Int64.to_int opt

let to_string_opt (opt, _) =
  match opt with
  | Some i64 -> Some (Int64.to_string i64)
  | None -> None

let to_float_opt (opt, _) = Option.map Int64.to_float opt
let visit ~v_tok (opt, t) = (opt, v_tok t)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let eq_const (opt, _) i2 =
  match opt with
  | None -> false
  | Some i1 -> Int64.equal i1 (Int64.of_int i2)

let eq_value (opt1, _) (opt2, _) = Option.equal Int64.equal opt1 opt2
