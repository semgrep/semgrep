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
*)

type t = private string
(** A lowercase hex-encoded string, represented as such in memory. *)

val compare : t -> t -> int
val equal : t -> t -> bool
val show : t -> string
val pp : Format.formatter -> t -> unit

val of_hex_string_opt : string -> t option
(** Validate and normalize *)

val to_hex_string : t -> string
(** Return the normalized representation (lowercase hexadecimal).
    [to_hex_string x] is equivalent to [(x :> string)] *)

val encode : string -> t
(** Encode an arbitrary string *)

val decode : t -> string
(** Decode a valid hex-encoded string *)
