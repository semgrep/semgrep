val parse :
  Fpath.t -> (AST_terraform.config, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern :
  string -> (AST_terraform.any, unit) Tree_sitter_run.Parsing_result.t
