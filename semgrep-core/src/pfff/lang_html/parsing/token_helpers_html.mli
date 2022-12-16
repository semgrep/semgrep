
val info_of_tok :
  Parser_html.token -> Ast_html.info
val visitor_info_of_tok :
  (Ast_html.info -> Ast_html.info) -> Parser_html.token -> Parser_html.token

val line_of_tok  : Parser_html.token -> int
val str_of_tok   : Parser_html.token -> string
val file_of_tok  : Parser_html.token -> Common.filename
val pos_of_tok   : Parser_html.token -> int
