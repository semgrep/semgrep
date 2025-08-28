(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
type t = Less | Equal | Greater

val binary_search_arr : f:(int -> 'a -> t) -> 'a array -> (int * 'a, int) result
(** [binary_search_arr f A] returns Ok (idx, x) if the element x can be found
    at idx x, according to comparison function f.
    Otherwise, it returns Error idx, where idx is the index that the element
    must be inserted at, if it were to be in the array.
    For instance, when searching for 2 in [|0, 3|], we get Error 1.
    Inserting at the beginning is Error 0, and at the end is Error 2.
  *)

val binary_search_bigarr1 :
  f:(int -> 'a -> t) -> ('a, 'b, 'c) Bigarray.Array1.t -> (int * 'a, int) result

val to_comparison : ('a -> 'a -> int) -> 'a -> 'a -> t
