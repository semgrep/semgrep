(*
   Sort rules topologically so as to print out type definitions in the
   order of their dependencies.

   This improves the clarity of the generated code.
   (there are also compile-time performance benefits when generating
   OCaml functions from type definitions)
*)

(* Group rules and sort them topologically, such that a rule may only
   reference earlier rules or rules in the same group.
   The boolean indicates whether the given rule references itself.
*)
val sort :
  CST_grammar.rule list ->
  (bool * CST_grammar.rule) list list
