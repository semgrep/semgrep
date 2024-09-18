(*
   Map tree-sitter-dockerfile CST to an AST.

   Note that this relies on Parse_bash_tree_sitter.ml to parse
   the bash constructs inside the Dockerfile.
*)

val parse :
  Fpath.t -> (AST_dockerfile.program, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern :
  string -> (AST_dockerfile.program, unit) Tree_sitter_run.Parsing_result.t

(*
   The input can be a sequence of dockerfile instructions
   or a bash snippet.
*)
val parse_docker_or_bash_pattern :
  string -> (AST_generic.any, unit) Tree_sitter_run.Parsing_result.t
