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
(** Type-unsafe wrappers around [Marshal] for reading/writing OCaml values
    to disk. The marshaled format is not stable across different compiler
    versions or builds.

    !! UNSAFE !! — the ['a] type parameter is unchecked at unmarshal time.
    Consider adding a version tag to detect schema mismatches. *)

val get_value : Fpath.t -> 'a
(** Reads a marshaled value from disk. *)

val write_value : 'a -> Fpath.t -> unit
(** Writes a value to disk. Raises if the value contains closures;
    use {!write_with_closures} instead in that case. *)

val write_with_closures : 'a -> Fpath.t -> unit
(** Like {!write_value}, but permits closures in the serialized value.
    The result can only be read back by the same binary; see
    https://ocaml.org/manual/5.3/api/Marshal.html#VALto_channel for
    other considerations. *)
