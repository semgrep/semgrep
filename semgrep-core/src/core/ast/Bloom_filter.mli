(* For now let's simplify and store only strings (e.g., from tokens)
 * inside the bloom filter. Later we can maybe make it polymorph.
 *)

type t

type elt = string

(* from deriving eq *)
val pp : Format.formatter -> t -> unit

val create : bool -> t

val is_empty : t -> bool

(* imperative interface *)
val add : elt -> t -> unit

(* bloom filter boolean for membership test *)
type bbool = No | Maybe

val mem : elt -> t -> bbool

(* for all elements in bf1, are they (Maybe) present in bf2 *)
val is_subset : elt list -> t -> bbool

val make_bloom_from_set : bool -> elt Set_.t -> t
