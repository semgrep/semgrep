
val visit_program:
  tag_hook:
    (Cst_js.tok -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  (*(Database_php.id * Common.filename * Database_php.database) option -> *)
  Cst_js.program * Parser_js.token list ->
  unit
