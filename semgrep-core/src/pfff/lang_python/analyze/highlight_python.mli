

val visit_program :
  tag_hook: (Parse_info.t -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  AST_python.program option * Parser_python.token list ->
  unit
