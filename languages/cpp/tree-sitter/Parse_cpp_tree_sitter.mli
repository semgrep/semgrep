val parse : Fpath.t -> (Ast_cpp.program, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern :
  string -> (Ast_cpp.any, unit) Tree_sitter_run.Parsing_result.t
