(* Brandon Wu
 *
 * Copyright (C) 2023 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Main functor *)
(*****************************************************************************)

(* OCaml has generative functors.
   This means that abstract types minted from different applications of the same
   functor are _always_ different, even if they are constructed in the exact
   same way.
   So different instances of the module `MkId` will have distinct types.
   This helps us not conflate types when we need a new kind of unique identifier type,
   and avoid boilerplate code with refs and counters.
*)
module MkId () : sig
  type t [@@deriving show, eq, ord, hash]

  val mk : unit -> t
  val to_int : t -> int
  val unsafe_default : t
  val is_unsafe_default : t -> bool
  val unsafe_reset_counter : unit -> unit
end = struct
  open Ppx_hash_lib.Std.Hash.Builtin

  type t = int [@@deriving show, eq, ord, hash]

  let counter = ref 0

  let mk () =
    incr counter;
    !counter

  let to_int = Fun.id
  let unsafe_default = -1
  let is_unsafe_default id = id = unsafe_default
  let unsafe_reset_counter () = counter := 0
end
