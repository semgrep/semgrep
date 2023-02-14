val parse :
  Common.filename -> AST_terraform.config Tree_sitter_run.Parsing_result.t

val parse_pattern : string -> AST_terraform.any Tree_sitter_run.Parsing_result.t
