
val is_eof          : Parser_ml.token -> bool
val is_comment      : Parser_ml.token -> bool

val token_kind_of_tok: Parser_ml.token -> Parse_info.token_kind

val info_of_tok :
  Parser_ml.token -> Parse_info.t
val visitor_info_of_tok :
  (Parse_info.t -> Parse_info.t) -> Parser_ml.token -> Parser_ml.token

val line_of_tok  : Parser_ml.token -> int
