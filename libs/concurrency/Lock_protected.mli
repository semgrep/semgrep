(* data protected by a lock *)
type 'a t

(* create a "protected" data *)
val protect : 'a -> 'a t

(* access the protected data; as the name suggest, this is protected by a lock
 * to avoid data-races
 *)
val with_lock : ('a -> 'b) -> 'a t -> 'b
