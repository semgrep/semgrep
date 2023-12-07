(* ??? *)
val unit_str : ?pad:bool -> int -> string -> string

val search : term:string -> string -> int option
(** [contains_opt term str] is a {b naive} implementation which tries to
    find [term] into the given [str]. It returns the position where it find
    the {b first} occurrence of [term]. Otherwise, it returns [None]. *)

(* [contains term str] returns true if [term] is inside [str] *)
val contains : term:string -> string -> bool
val empty : string -> bool
val split : sep:string (* regexp *) -> string -> string list
