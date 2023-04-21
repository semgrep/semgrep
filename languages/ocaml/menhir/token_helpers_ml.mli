val is_eof : Parser_ml.token -> bool
val is_comment : Parser_ml.token -> bool
val token_kind_of_tok : Parser_ml.token -> Lib_ast_fuzzy.token_kind
val info_of_tok : Parser_ml.token -> Tok.t
val visitor_info_of_tok : (Tok.t -> Tok.t) -> Parser_ml.token -> Parser_ml.token
val line_of_tok : Parser_ml.token -> int
