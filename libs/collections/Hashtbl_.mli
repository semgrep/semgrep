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
(*
   Extension of the standard library module Hashtbl
*)

type hash = int

val combine_hash : hash -> hash -> hash
val hash_of_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
val hash_to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
val hkeys : ('a, 'b) Hashtbl.t -> 'a list
val map : ('k -> 'v -> 'w) -> ('k, 'v) Hashtbl.t -> ('k, 'w) Hashtbl.t

val sorted_iter :
  cmp:('a -> 'a -> hash) -> ('a -> 'b -> unit) -> ('a, 'b) Hashtbl.t -> unit

type 'a hashset = ('a, bool) Hashtbl.t

val hashset_of_list : 'a list -> 'a hashset
val hashset_to_list : 'a hashset -> 'a list

val push : ('k, 'v list ref) Hashtbl.t -> 'k -> 'v -> unit
(** add a value to the stack associated with a key. *)

val peek_opt : ('k, 'v list ref) Hashtbl.t -> 'k -> 'v option
(** peek a value at the top of the stack associated with a key.
   Returns None if the key is unbound.
 *)

val get_stack : ('k, 'v list ref) Hashtbl.t -> 'k -> 'v list
(** get the stack associated with a key. Values are returned as
   a list, most recently-added first. Returns an empty list
   if the key is unbound.
*)

val update_default :
  'k -> update:('v -> 'v) -> default:(unit -> 'v) -> ('k, 'v) Hashtbl.t -> unit
(** attempts to update the element assositaed with the key.
  if the key doesn't exist, then updates the hashtable with the mapping
  key -> update (default ())
 *)

(* Note: [Base] shadows the library module [Base] for the rest of this file.
   Nothing follows this module here, and inside its own signature below,
   unqualified [Base] still refers to the library (a module can't refer to
   itself before its own definition is complete). *)
module Base : sig
  (** [Base.Hashtbl.t]-flavored counterparts of the helpers above, with the
      same names and argument orders, so a call site migrates by changing
      only its module qualifier. *)

  val find : ('k, 'v) Base.Hashtbl.t -> 'k -> 'v
  (** like [Stdlib.Hashtbl.find]: @raise Not_found when absent *)

  val hash_of_list : ('k * 'v) list -> ('k, 'v) Base.Hashtbl.t
  (** last binding for a given key wins *)

  val hash_to_list : ('k, 'v) Base.Hashtbl.t -> ('k * 'v) list
  (** sorted by [compare] on the (key, data) pair *)

  val hkeys : ('k, _) Base.Hashtbl.t -> 'k list
  (** sorted; each key appears once *)

  val map :
    ('k -> 'v -> 'w) -> ('k, 'v) Base.Hashtbl.t -> ('k, 'w) Base.Hashtbl.t

  val sorted_iter :
    cmp:('k -> 'k -> int) ->
    ('k -> 'v -> unit) ->
    ('k, 'v) Base.Hashtbl.t ->
    unit

  type 'a hashset = ('a, bool) Base.Hashtbl.t

  val hashset_of_list : 'a list -> 'a hashset

  val hashset_to_list : 'a hashset -> 'a list
  (** sorted *)

  val push : ('k, 'v list ref) Base.Hashtbl.t -> 'k -> 'v -> unit
  (** add a value to the stack associated with a key. *)

  val peek_opt : ('k, 'v list ref) Base.Hashtbl.t -> 'k -> 'v option
  (** peek a value at the top of the stack associated with a key.
     Returns None if the key is unbound.
   *)

  val get_stack : ('k, 'v list ref) Base.Hashtbl.t -> 'k -> 'v list
  (** get the stack associated with a key. Values are returned as
     a list, most recently-added first. Returns an empty list
     if the key is unbound.
  *)

  val find_default : 'k -> (unit -> 'v) -> ('k, 'v) Base.Hashtbl.t -> 'v
  (** like [find], but inserts and returns the default thunk's result when
      the key is absent, instead of raising. *)

  val update_default :
    'k ->
    update:('v -> 'v) ->
    default:(unit -> 'v) ->
    ('k, 'v) Base.Hashtbl.t ->
    unit
  (** attempts to update the element associated with the key.
    if the key doesn't exist, then updates the hashtable with the mapping
    key -> update (default ())
   *)
end
