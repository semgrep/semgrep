(*
   Parse a typescript program into a javascript AST.

   The plan is to enrich the javascript AST progressively so as to support
   the full typescript language.

   We also want to support tsx (React syntax for typescript) which comes
   as a slightly different grammar and CST than typescript.
*)

type dialect = [ `Typescript | `TSX ]

(*
   Parse a file as pure typescript or as TSX. If unspecified, the
   dialect is guessed from the file extension. Pure typescript is the fallback
   if the extension is unknown.
*)
val parse :
  ?dialect:dialect ->
  string ->
  Ast_js.a_program Tree_sitter_run.Parsing_result.t
