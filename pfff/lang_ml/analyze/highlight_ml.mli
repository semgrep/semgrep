
val visit_program:
  ?lexer_based_tagger:bool ->
  tag_hook:
    (Parse_info.t -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  (*(Database_php.id * Common.filename * Database_php.database) option -> *)
  Cst_ml.program * Parser_ml.token list ->
  unit
