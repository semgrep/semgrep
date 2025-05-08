type ('a, 'b) t = ('a * 'b) list

val keys : ('a, 'b) t -> 'a list
val join_keys : ('a, 'b) t -> ('a, 'c) t -> 'a list
val find_opt : 'a -> ('a, 'b) t -> 'b option

(* sorts *)
val sort_by_val_lowfirst : ('a, 'b) t -> ('a * 'b) list
val sort_by_val_highfirst : ('a, 'b) t -> ('a * 'b) list
val sort_by_key_lowfirst : ('a, 'b) t -> ('a * 'b) list
val sort_by_key_highfirst : ('a, 'b) t -> ('a * 'b) list

(* group by *)
val group_by : ('a -> 'b) -> 'a list -> ('b, 'a list) t
val group_assoc_bykey_eff : ('a, 'b) t -> ('a, 'b list) t
val group_by_mapped_key : ('a -> 'b) -> 'a list -> ('b, 'a list) t
val group_by_multi : ('a -> 'b list) -> 'a list -> ('b, 'a list) t
