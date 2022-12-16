(*s: token_helpers_php.mli *)
val is_eof          : Parser_php.token -> bool
val is_comment      : Parser_php.token -> bool
val is_just_comment : Parser_php.token -> bool

val token_kind_of_tok: Parser_php.token -> Parse_info.token_kind

(*x: token_helpers_php.mli *)
val info_of_tok :
  Parser_php.token -> Parse_info.t
val visitor_info_of_tok :
  (Parse_info.t -> Parse_info.t) -> Parser_php.token -> Parser_php.token
(*x: token_helpers_php.mli *)
val line_of_tok  : Parser_php.token -> int
(*x: token_helpers_php.mli *)

(*e: token_helpers_php.mli *)
