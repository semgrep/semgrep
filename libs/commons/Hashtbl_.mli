val hash_of_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
val hash_to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
val hkeys : ('a, 'b) Hashtbl.t -> 'a list

type 'a hashset = ('a, bool) Hashtbl.t

val hashset_of_list : 'a list -> 'a hashset
val hashset_to_list : 'a hashset -> 'a list
