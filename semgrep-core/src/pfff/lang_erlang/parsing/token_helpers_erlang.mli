
val is_eof          : Parser_erlang.token -> bool
val is_comment      : Parser_erlang.token -> bool

val info_of_tok :
  Parser_erlang.token -> Parse_info.t
val visitor_info_of_tok :
  (Parse_info.t -> Parse_info.t) -> Parser_erlang.token -> Parser_erlang.token
