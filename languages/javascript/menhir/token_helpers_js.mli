val is_eof : Parser_js.token -> bool
val is_comment : Parser_js.token -> bool
val token_kind_of_tok : Parser_js.token -> Lib_ast_fuzzy.token_kind
val info_of_tok : Parser_js.token -> Tok.t
val visitor_info_of_tok : (Tok.t -> Tok.t) -> Parser_js.token -> Parser_js.token
val line_of_tok : Parser_js.token -> int
val col_of_tok : Parser_js.token -> int
