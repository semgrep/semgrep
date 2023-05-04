val is_eof : Parser_ruby.token -> bool
val is_comment : Parser_ruby.token -> bool
val info_of_tok : Parser_ruby.token -> Tok.t

val visitor_info_of_tok :
  (Tok.t -> Tok.t) -> Parser_ruby.token -> Parser_ruby.token
