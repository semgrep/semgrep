(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
type ('a, 'b) t = ('a * 'b) list

val keys : ('a, 'b) t -> 'a list
val join_keys : ('a, 'b) t -> ('a, 'c) t -> 'a list
val find_opt : 'a -> ('a, 'b) t -> 'b option

(* sorts *)
val sort_by_val_lowfirst : ('a, 'b) t -> ('a * 'b) list
val sort_by_val_highfirst : ('a, 'b) t -> ('a * 'b) list
val sort_by_key_lowfirst : ('a, 'b) t -> ('a * 'b) list
val sort_by_key_highfirst : ('a, 'b) t -> ('a * 'b) list

(* group by *)
val group_by : ('a -> 'b) -> 'a list -> ('b, 'a list) t
val group_assoc_bykey_eff : ('a, 'b) t -> ('a, 'b list) t
val group_by_mapped_key : ('a -> 'b) -> 'a list -> ('b, 'a list) t
val group_by_multi : ('a -> 'b list) -> 'a list -> ('b, 'a list) t
