(*
   Parse a typescript program into a javascript AST.

   The plan is to enrich the javascript AST progressively so as to support
   the full typescript language.

   We also want to support tsx (React syntax for typescript) which comes
   as a slightly different grammar and CST than typescript.
*)

type dialect = [ `Typescript | `TSX ]

val parse : dialect -> string -> Ast_js.program
