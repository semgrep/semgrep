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
(* Note that since OCaml 4.12.0, the standard library has an Either module
 * but it is not recognized by default by ppx_deriving so it's simpler
 * for now to define our own alias with the right deriving.
 *)

(* Haskell-inspired either type *)
type ('a, 'b) t = ('a, 'b) Either.t = Left of 'a | Right of 'b
[@@deriving eq, show, sexp]

val partition : ('a -> ('b, 'c) t) -> 'a list -> 'b list * 'c list

type ('a, 'b, 'c) either3 = Left3 of 'a | Middle3 of 'b | Right3 of 'c
[@@deriving eq, show]

val partition_either3 :
  ('a -> ('b, 'c, 'd) either3) -> 'a list -> 'b list * 'c list * 'd list
