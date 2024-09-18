val parse : Fpath.t -> (AST_ql.program, unit) Tree_sitter_run.Parsing_result.t

val parse_string :
  file:string (* filename *) ->
  contents:string ->
  (AST_ql.program, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern :
  string -> (AST_ql.any, unit) Tree_sitter_run.Parsing_result.t
