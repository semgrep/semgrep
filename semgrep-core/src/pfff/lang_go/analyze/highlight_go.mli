
val visit_program :
  tag_hook: (Parse_info.t -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  Ast_go.program * Parser_go.token list ->
  unit
