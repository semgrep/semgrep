
val visit_program :
  tag: (Cst_php.info -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  (string, Database_code.entity) Hashtbl.t -> 
  Cst_php.program * Parser_php.token list ->
  unit
