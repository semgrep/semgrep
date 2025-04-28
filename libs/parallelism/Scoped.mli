(* Nathan Taylor
 *
 * Copyright (C) 2025 Semgrep Inc.
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

type 'a t

val create : 'a -> 'a t
val get : 'a t -> 'a
val with_hook_set : 'a t -> 'a -> (unit -> 'b) -> 'b
val with_ : 'a t -> 'a -> (unit -> 'b) -> unit -> 'b
