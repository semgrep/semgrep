(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Encode and decode strings to hexadecimal.

   Wrapper around the Hex library (whose interface is weird).

   We assume the Hex library enforces a consistent encoding for a given
   string (a = b <=> hex a = hex b).
*)

(* A guaranteed lowercase hex representation of a string *)
type t = string

let equal = String.equal
let compare = String.compare
let show x = x
let pp fmt x = Format.pp_print_string fmt x

(* Re-encode:
   - validate
   - normalize
*)
let of_hex_string_opt orig_hex_data =
  try
    let decoded = Hex.to_string (`Hex orig_hex_data) in
    let (`Hex encoded) = Hex.of_string decoded in
    Some encoded
  with
  | _ -> None

(* Turn the private type into a string (see mli) *)
let to_hex_string x = x

let encode data =
  let (`Hex encoded) = Hex.of_string data in
  encoded

(* This won't fail because the module interface guarantees that hex_data
   is well-formed. *)
let decode hex_data = Hex.to_string (`Hex hex_data)
