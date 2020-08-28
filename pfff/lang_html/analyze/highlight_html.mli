
val visit_toplevel :
  tag_hook:
    (Ast_html.info -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  (*(Database_php.id * Common.filename * Database_php.database) option -> *)
  Ast_html.html_tree * Parser_html.token list ->
  unit
