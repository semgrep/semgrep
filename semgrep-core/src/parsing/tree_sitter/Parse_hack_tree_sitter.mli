(*
   Parse a Hack file using the tree-sitter parser and convert the CST
   to the PHP/Hack AST.
*)

(* TODO: make this type reusable for other languages? *)
type input_kind = [ `Pattern | `Target ]

(*
   Parse a file.
*)
val parse :
  input_kind ->
  Common.filename ->
  Ast_php.program Tree_sitter_run.Parsing_result.t

(*
   Parse a string, using a temporary file under the hood.
   This is meant for parsing semgrep patterns, which are typically short.

   Unlike 'parse', this can return a fragment of a program such as a single
   expression that would be an invalid program but makes a useful pattern
   to search for.
*)
val any_of_string : input_kind -> string -> Ast_php.any
