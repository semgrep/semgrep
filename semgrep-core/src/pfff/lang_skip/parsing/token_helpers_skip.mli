
val is_eof          : Parser_skip.token -> bool
val is_comment      : Parser_skip.token -> bool

val token_kind_of_tok: Parser_skip.token -> Parse_info.token_kind

val info_of_tok :
  Parser_skip.token -> Parse_info.t
val visitor_info_of_tok :
  (Parse_info.t -> Parse_info.t) -> Parser_skip.token -> Parser_skip.token
