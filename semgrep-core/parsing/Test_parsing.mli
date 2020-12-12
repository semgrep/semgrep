
val parsing_stats: Lang.t -> bool (* json *) ->
  (Common.filename list -> Common.filename list) -> Common.filename list ->
  unit

val parsing_regressions: Lang.t ->
  (Common.filename list -> Common.filename list) -> Common.filename list ->
  unit

val test_parse_tree_sitter: string -> Common.filename list -> unit

val dump_tree_sitter_cst: Common.filename -> unit
val dump_ast_pfff: Common.filename -> unit
val diff_pfff_tree_sitter: Common.filename list -> unit
