val parse :
  ((* to parse the JS inside <script> *)
   Common.filename -> AST_generic.program) ->
  Common.filename ->
  AST_generic.program Tree_sitter_run.Parsing_result.t
