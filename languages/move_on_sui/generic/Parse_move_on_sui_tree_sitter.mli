val parse :
  Fpath.t -> (AST_generic.program, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern :
  string -> (AST_generic.any, unit) Tree_sitter_run.Parsing_result.t
