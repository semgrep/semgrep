val is_eof : Parser_python.token -> bool
val is_comment : Parser_python.token -> bool
val info_of_tok : Parser_python.token -> Tok.t

val visitor_info_of_tok :
  (Tok.t -> Tok.t) -> Parser_python.token -> Parser_python.token
