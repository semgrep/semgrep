type t

val of_string : string -> t
val to_string : t -> string
val combine : ?sep:string -> t list -> t
