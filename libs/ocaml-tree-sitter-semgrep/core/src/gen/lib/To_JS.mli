(*
   Convert a JSON grammar to JavaScript for better readability when
   debugging.
*)

(* Usage: run (Some "grammar.json") (Some "grammar.js")

   sort_choices: sort the elements of the 'choice()' constructs.
   This normalization may not completely preserve the same parsing behavior
   but in general it should and it's convenient for comparing two grammars.

   sort_rules: sort all the rule definitions alphabetically except for the
   first one because it must stay in place to be identified by tree-sitter
   as the grammar's entry point.
*)
val run :
  sort_choices:bool ->
  sort_rules:bool ->
  strip:bool ->
  string option -> string option -> unit
