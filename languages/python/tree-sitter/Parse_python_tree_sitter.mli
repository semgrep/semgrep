val parse :
  Fpath.t -> (AST_python.program, unit) Tree_sitter_run.Parsing_result.t

val parse_string :
  file:string (* filename *) ->
  contents:string ->
  (AST_python.program, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern :
  string -> (AST_python.any, unit) Tree_sitter_run.Parsing_result.t
