(* For now let's simplify and store only strings (e.g., from tokens)
 * inside the bloom filter. Later we can maybe make it polymorph.
 *)

type t

type elt = string

(* from deriving eq *)
val pp : Format.formatter -> t -> unit

val create : t

val is_empty : t -> bool

val add : elt -> t -> t

val mem : elt -> t -> bool

val set_of_filter : t -> elt Set_.t

val is_subset : elt Set_.t -> t -> bool

val make_bloom_from_set : elt Set_.t -> t
