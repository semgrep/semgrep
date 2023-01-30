(*
   Map tree-sitter-dockerfile CST to an AST.
*)

val parse :
  Common.filename -> AST_dockerfile.program Tree_sitter_run.Parsing_result.t

val parse_pattern :
  string -> AST_dockerfile.program Tree_sitter_run.Parsing_result.t

(*
   The input can be a sequence of dockerfile instructions
   or a bash snippet.
*)
val parse_docker_or_bash_pattern :
  string -> AST_generic.any Tree_sitter_run.Parsing_result.t
