(*
   OCaml-friendly representation of parse trees produced by tree-sitter.
*)

(* Convert the C API tree to a convenient OCaml tree. *)
val of_ts_tree : Tree_sitter_API.ts_tree -> Tree_sitter_output_t.node

(*
   Convert the C API tree to json. This contains at least all the original
   data.

   The output is nicely indented by default, which can be very slow on
   large input. Use '~pretty:false' to disable pretty-printing.
*)
val to_json : ?pretty:bool -> Tree_sitter_output_t.node -> string
