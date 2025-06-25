val parse_pattern :
  ?rule_options:Rule_options_t.t ->
  Lang.t ->
  string ->
  (Pattern.t, string) Result.t

val dump_tree_sitter_pattern_cst : Lang.t -> Fpath.t -> unit
