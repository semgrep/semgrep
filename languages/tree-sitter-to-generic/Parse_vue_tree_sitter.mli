val parse :
  ((* to parse the JS inside <script> *)
   string (* filename *) ->
  AST_generic.program) ->
  string (* filename *) ->
  AST_generic.program Tree_sitter_run.Parsing_result.t
