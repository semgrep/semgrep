(*
   Generic functions used by more than one module in this library
*)

(* Safe version of List.map for ocaml < 5 *)
val list_map : ('a -> 'b) -> 'a list -> 'b list

(* Safe version of List.flatten *)
val list_flatten : 'a list list -> 'a list

(* Create a directory if it doesn't exist.
   Also create its parents if they don't exist and 'recursive' is true.
*)
val make_dir_if_not_exists : ?recursive:bool -> string -> unit
