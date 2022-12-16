
val is_eof: Parser_css.token -> bool
val is_comment: Parser_css.token -> bool

val info_of_tok :
  Parser_css.token -> Ast_css.info
val visitor_info_of_tok :
  (Ast_css.info -> Ast_css.info) -> Parser_css.token -> Parser_css.token
