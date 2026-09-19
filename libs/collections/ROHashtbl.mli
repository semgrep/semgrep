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
type ('a, 'b) t
(** A read-only hash table. NOT an immutable hash table. Mutable references to
    this table may still exist, and it could be mutated. *)

val of_hashtbl : ('a, 'b) Hashtbl.t -> ('a, 'b) t
(** Convert from an ordinary hash table to a read-only hash table. O(1). *)

val create : unit -> ('a, 'b) t
(** Creates an empty, read-only hash table. Same as [Hashtbl.create] but there is
    no point having an initial size parameter because this table will always be
    empty. *)

val find : ('a, 'b) t -> 'a -> 'b
(** See https://ocaml.org/manual/5.3/api/Hashtbl.html for the behavior of all
    functions below. *)

val find_opt : ('a, 'b) t -> 'a -> 'b option
val mem : ('a, 'b) t -> 'a -> bool
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
val fold : ('a -> 'b -> 'acc -> 'acc) -> ('a, 'b) t -> 'acc -> 'acc
val length : ('a, 'b) t -> int
val to_seq : ('a, 'b) t -> ('a * 'b) Seq.t
val to_seq_keys : ('a, _) t -> 'a Seq.t
val to_seq_values : (_, 'b) t -> 'b Seq.t
val of_seq : ('a * 'b) Seq.t -> ('a, 'b) t

(** Note: [Base] shadows the library module [Base] for the rest of this file.
    Nothing follows this module here, and inside its own signature below,
    unqualified [Base] still refers to the library (a module can't refer to
    itself before its own definition is complete). *)
module Base : sig
  type ('a, 'b) t
  (** [Base.Hashtbl.t]-flavored counterpart of the read-only hash table above.
      Most read-API shapes are identical to the stdlib flavor, so a call site
      generally migrates by changing only its module qualifier:
      [ROHashtbl.] -> [ROHashtbl.Base.] *)

  val of_hashtbl : ('a, 'b) Base.Hashtbl.t -> ('a, 'b) t
  (** Convert from an ordinary hash table to a read-only hash table. O(1),
      never copies. *)

  val create : unit -> ('a, 'b) t
  (** Creates an empty, read-only hash table. *)

  val of_seq : ('a * 'b) Seq.t -> ('a, 'b) t
  (** Like [Stdlib.Hashtbl.of_seq]: last binding for a given key wins. *)

  val find : ('a, 'b) t -> 'a -> 'b
  (** See https://ocaml.org/manual/5.3/api/Hashtbl.html for the behavior of all
      functions below. [find] raises [Not_found], like [Stdlib.Hashtbl.find]. *)

  val find_opt : ('a, 'b) t -> 'a -> 'b option
  val mem : ('a, 'b) t -> 'a -> bool
  val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
  val fold : ('a -> 'b -> 'acc -> 'acc) -> ('a, 'b) t -> 'acc -> 'acc
  val length : ('a, 'b) t -> int
  val to_alist : ('a, 'b) t -> ('a * 'b) list
  val keys : ('a, _) t -> 'a list
  val data : (_, 'b) t -> 'b list
end
