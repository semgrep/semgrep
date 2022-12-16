
val is_eof          : Parser_go.token -> bool
val is_irrelevant      : Parser_go.token -> bool
val is_comment_or_space      : Parser_go.token -> bool

val info_of_tok :
  Parser_go.token -> Parse_info.t
val visitor_info_of_tok :
  (Parse_info.t -> Parse_info.t) -> Parser_go.token -> Parser_go.token

val token_kind_of_tok: Parser_go.token -> Parse_info.token_kind
