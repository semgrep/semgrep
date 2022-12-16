
val visit_program :
  tag_hook:
    (Parse_info.t -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  Ast_hs.program * Parser_hs.token list ->
  unit
