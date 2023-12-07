(*
   Test tags

   Tags are strings which is nice and extensible, but to prevent
   misspellings and conflicts, we require them to be registered
   using 'Tag.declare'.
*)
type t = private string

(* Create a tag. Each tag may only be created once. *)
val declare : string -> t
val list : unit -> t list
val compare : t -> t -> int
val equal : t -> t -> bool
val show : t -> string
val to_string : t -> string
