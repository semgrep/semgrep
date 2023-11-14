val parse :
  Common.filename -> AST_python.program Tree_sitter_run.Parsing_result.t

val parse_pattern : string -> AST_python.any Tree_sitter_run.Parsing_result.t
