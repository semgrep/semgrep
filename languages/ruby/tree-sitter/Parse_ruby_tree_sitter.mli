val parse : Fpath.t -> (Ast_ruby.program, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern :
  string -> (Ast_ruby.any, unit) Tree_sitter_run.Parsing_result.t
