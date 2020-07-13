
(* This uses either the pfff or tree-sitter parsers.
 * This also resolve names and propagate constants.
 *)
val parse_and_resolve_name_use_pfff_or_treesitter:
  Lang.t -> Common.filename -> AST_generic.program
