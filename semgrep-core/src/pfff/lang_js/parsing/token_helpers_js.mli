
val is_eof          : Parser_js.token -> bool
val is_comment      : Parser_js.token -> bool

val token_kind_of_tok: Parser_js.token -> Parse_info.token_kind

val info_of_tok :
  Parser_js.token -> Parse_info.t
val visitor_info_of_tok :
  (Parse_info.t -> Parse_info.t) -> Parser_js.token -> Parser_js.token

val line_of_tok  : Parser_js.token -> int
val col_of_tok  : Parser_js.token -> int
