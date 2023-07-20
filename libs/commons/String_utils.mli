(* ??? *)
val unit_str : ?pad:bool -> int -> string -> string

val contains : string -> string -> int option
(** [contains term str] is a {b naive} implementation which tries to find [term]
    into the given [str]. It returns the position where it find the {b first}
    occurrence of [term]. Otherwise, it returns [None]. *)
