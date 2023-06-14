val is_eof : Parser_php.token -> bool
val is_comment : Parser_php.token -> bool
val is_just_comment : Parser_php.token -> bool
val token_kind_of_tok : Parser_php.token -> Lib_ast_fuzzy.token_kind
val info_of_tok : Parser_php.token -> Tok.t

val visitor_info_of_tok :
  (Tok.t -> Tok.t) -> Parser_php.token -> Parser_php.token

val line_of_tok : Parser_php.token -> int
