val is_eof : Parser_java.token -> bool
val is_comment : Parser_java.token -> bool
val is_just_comment : Parser_java.token -> bool
val token_kind_of_tok : Parser_java.token -> Lib_ast_fuzzy.token_kind
val info_of_tok : Parser_java.token -> Tok.t

val visitor_info_of_tok :
  (Tok.t -> Tok.t) -> Parser_java.token -> Parser_java.token

val line_of_tok : Parser_java.token -> int
