(* Provides a (more) typesafe facility for marshaling and unmarshaling values
 * during a single run of an OCaml program. Clients can marshal into memory,
 * enjoying the memory savings of a more compact representation for values that
 * are not frequently used. Alternatively, clients may marshal values to disk.
 * *)

module InMemory : sig
  (* marshaled representation of 'a *)
  type 'a t

  val marshal : 'a -> 'a t
  val unmarshal : 'a t -> 'a
end

module OnDisk : sig
  (* path of the marshaled file on disk (in a cache directory) *)
  type 'a t

  val marshal : Fpath.t -> 'a -> 'a t
  val unmarshal : 'a t -> 'a
  val get_path : 'a t -> Fpath.t
end
