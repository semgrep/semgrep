(*
   Print function definitions how we like them.
*)

(* Print OCaml code to stdout. *)
val generate_boilerplate :
  Conf.t -> AST_ocaml.type_declaration list list -> unit
