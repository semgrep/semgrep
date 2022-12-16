
val visit_program :
  tag: (Parse_info.t -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  (string, Database_code.entity) Hashtbl.t ->
  Cst_php.program * Parser_php.token list ->
  unit
