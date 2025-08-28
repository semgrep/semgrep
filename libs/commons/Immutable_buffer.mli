(*
   Copyright (c) 2022-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Similar in principle to OCaml's Buffer, but immutable. `combine` is `O(1)`
 * (though the construction of the list to pass to it is `O(n)` where `n` is the
 * number of list items), and `to_string` is `O(n)` where `n` is the length of
 * the constructed string. *)

type t

val of_string : string -> t
val to_string : t -> string
val combine : ?sep:string -> t list -> t
