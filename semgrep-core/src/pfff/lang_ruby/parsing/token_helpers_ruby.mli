
val is_eof          : Parser_ruby.token -> bool
val is_comment      : Parser_ruby.token -> bool

val info_of_tok :
  Parser_ruby.token -> Parse_info.t
val visitor_info_of_tok :
  (Parse_info.t -> Parse_info.t) -> Parser_ruby.token -> Parser_ruby.token
