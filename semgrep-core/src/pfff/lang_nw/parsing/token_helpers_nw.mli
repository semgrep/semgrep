
val is_eof          : Lexer_nw.token -> bool
val is_comment      : Lexer_nw.token -> bool

val token_kind_of_tok: Lexer_nw.token -> Parse_info.token_kind

val info_of_tok :
  Lexer_nw.token -> Parse_info.t
val visitor_info_of_tok :
  (Parse_info.t -> Parse_info.t) -> Lexer_nw.token -> Lexer_nw.token
