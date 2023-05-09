(*
   OCaml implementation of realpath. Remove once we depend on ocaml >= 4.13.
*)

val realpath : Fpath.t -> Fpath.t
val realpath_str : string -> string
