(* Andre Kuhlenschmidt
 *
 * Copyright (C) 2023 r2c
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

(* This library implements a bitfield that can be stored as an
 * immediate in ocaml memory representation. Requiring no memory
 * indirection. As such, it is an immutable type. It should allow 31-63
 * boolean flags to be stored without indirection or allocation. *)

type t

val empty : t
val num_bits : int
val max_bit : int
val min_bit : int

exception Index_exn of int

val get_bit : t -> int -> bool
val set_bit : t -> int -> bool -> t
val show : t -> string
val equal : t -> t -> bool
val hash : t -> Base.Hash.hash_value
val hash_fold_t : Base.Hash.state -> t -> Base.Hash.state
