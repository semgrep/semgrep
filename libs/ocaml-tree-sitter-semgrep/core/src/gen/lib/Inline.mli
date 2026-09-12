(*
   Transform the grammar's original type definitions so as to inline
   some of them. This is meant to make the OCaml types more readable.
*)

(*
   Replace type names by their value if they're used only once.
   Their definition is then marked as inlined and is moved to the
   bottom of the list of definitions, since no other type definition
   depends on them anymore.

   Note that the types that are inlined in the OCaml CST still occur
   as named nodes (SYMBOL) in tree-sitter's output.
*)
val inline_rules : CST_grammar.t -> CST_grammar.t
