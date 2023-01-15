(* Similar in principle to OCaml's Buffer, but immutable. `combine` is `O(1)`
 * (though the construction of the list to pass to it is `O(n)` where `n` is the
 * number of list items), and `to_string` is `O(n)` where `n` is the length of
 * the constructed string. *)

type t

val of_string : string -> t
val to_string : t -> string
val combine : ?sep:string -> t list -> t
