
val visit_program :
  tag_hook: (Parse_info.t -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  Ast_ruby.program option * Parser_ruby.token list ->
  unit
