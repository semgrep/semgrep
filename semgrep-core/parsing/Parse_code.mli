
(* This uses either the pfff or tree-sitter parsers.
 * This also resolve names and propagate constants.
 *)
val parse_and_resolve_name_use_pfff_or_treesitter:
  Lang.t -> Common.filename -> AST_generic.program * Error_code.error list

(* used only for testing purpose *)
val just_parse_with_lang:
  Lang.t -> Common.filename -> AST_generic.program * Error_code.error list
