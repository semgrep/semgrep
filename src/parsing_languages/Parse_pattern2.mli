val parse_pattern :
  Lang.t -> bool (* print_errors *) -> string -> AST_generic.any

val dump_tree_sitter_pattern_cst : Lang.t -> Common.filename -> unit
