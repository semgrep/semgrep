val parse_pattern :
  Rule_options_t.t option -> Lang.t -> string -> AST_generic.any

val dump_tree_sitter_pattern_cst : Lang.t -> Fpath.t -> unit
