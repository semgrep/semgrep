val parse_pattern :
  bool (* print_errors *) -> Lang.t -> string -> AST_generic.any

val dump_tree_sitter_pattern_cst : Lang.t -> Common.filename -> unit
