

val visit_toplevel :
  tag_hook:
    (Parse_info.t -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  (*(Database_php.id * Common.filename * Database_php.database) option -> *)
  Ast_cpp.program * Parser_cpp.token list ->
  unit
