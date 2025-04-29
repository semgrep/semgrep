(*
   Extension of the standard library module Hashtbl
*)

type hash = int

val combine_hash : hash -> hash -> hash
val hash_of_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
val hash_to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
val hkeys : ('a, 'b) Hashtbl.t -> 'a list
val map : ('k -> 'v -> 'w) -> ('k, 'v) Hashtbl.t -> ('k, 'w) Hashtbl.t

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
