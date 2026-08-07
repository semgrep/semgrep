(*
   Copyright (c) 2024-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Provides a (more) typesafe facility for marshaling and unmarshaling values
 * during a single run of an OCaml program. Clients can marshal into memory,
 * enjoying the memory savings of a more compact representation for values that
 * are not frequently used. Alternatively, clients may marshal values to disk.
 *)

module InMemory : sig
  (* marshaled representation of 'a *)
  type 'a t

  (* [flags] is forwarded to [Marshal.to_string]. Pass [Marshal.Closures] to
   * marshal values that contain function values; such blobs are only safe to
   * unmarshal within the same running binary. *)
  val marshal : ?flags:Marshal.extern_flags list -> 'a -> 'a t
  val unmarshal : 'a t -> 'a

  (* Size in bytes of the marshaled representation. Useful to account for the
   * heap footprint of the compact form. *)
  val size_bytes : 'a t -> int
end

module OnDisk : sig
  (* path of the marshaled file on disk (in a cache directory) *)
  type 'a t

  val marshal : Fpath.t -> 'a -> 'a t
  val unmarshal : 'a t -> 'a
  val get_path : 'a t -> Fpath.t
end
