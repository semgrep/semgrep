(* A read-only hash table. NOT an immutable hash table. Mutable references to
 * this table may still exist, and it could be mutated. *)
type ('a, 'b) t

(* Convert from an ordinary hash table to a read-only hash table. O(1). *)
val of_hashtbl : ('a, 'b) Hashtbl.t -> ('a, 'b) t

(* Creates an empty, read-only hash table. Same as `Hashtbl.create` but there is
 * no point having an initial size parameter because this table will always be
 * empty. *)
val create : unit -> ('a, 'b) t

(* Same as Hashtbl.copy. Returns a mutable `Hashtbl.t`. This is an escape hatch
 * if you need a mutable copy of a read-only hash table. *)
val copy : ('a, 'b) t -> ('a, 'b) Hashtbl.t

(* See https://ocaml.org/manual/5.3/api/Hashtbl.html for the behavior of all
 * functions below *)
val find : ('a, 'b) t -> 'a -> 'b
val find_opt : ('a, 'b) t -> 'a -> 'b option
val find_all : ('a, 'b) t -> 'a -> 'b list
val mem : ('a, 'b) t -> 'a -> bool
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
val fold : ('a -> 'b -> 'acc -> 'acc) -> ('a, 'b) t -> 'acc -> 'acc
val length : ('a, 'b) t -> int
val is_randomized : unit -> bool
val rebuild : ?random:bool -> ('a, 'b) t -> ('a, 'b) t
val stats : ('a, 'b) t -> Hashtbl.statistics
val to_seq : ('a, 'b) t -> ('a * 'b) Seq.t
val to_seq_keys : ('a, _) t -> 'a Seq.t
val to_seq_values : (_, 'b) t -> 'b Seq.t
val of_seq : ('a * 'b) Seq.t -> ('a, 'b) t
