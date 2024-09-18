(* the first argument is to parse the JS inside <script> *)
val parse :
  (Fpath.t -> AST_generic.program) ->
  Fpath.t ->
  (AST_generic.program, unit) Tree_sitter_run.Parsing_result.t
