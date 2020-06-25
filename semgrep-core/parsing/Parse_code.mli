
(* This uses either the pfff or tree-sitter parsers.
 * This also resolve names and propagate constants.
 *)
val parse_with_lang: Lang.t -> Common.filename -> AST_generic.program
