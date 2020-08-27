(*
   Public interface for parsing javascript with the tree-sitter parser.
*)

val parse: Common.filename -> Ast_js.program
