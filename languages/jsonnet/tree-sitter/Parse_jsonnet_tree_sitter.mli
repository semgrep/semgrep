val parse :
  Fpath.t -> (AST_jsonnet.program, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern :
  string -> (AST_jsonnet.any, unit) Tree_sitter_run.Parsing_result.t
