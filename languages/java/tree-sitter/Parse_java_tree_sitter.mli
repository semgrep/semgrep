val parse : Fpath.t -> (Ast_java.program, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern :
  string -> (Ast_java.any, unit) Tree_sitter_run.Parsing_result.t
