(*s: pfff/lang_python/parsing/Token_helpers_python.mli *)

(*s: signature [[Token_helpers_python.is_eof]] *)
val is_eof          : Parser_python.token -> bool
(*e: signature [[Token_helpers_python.is_eof]] *)
(*s: signature [[Token_helpers_python.is_comment]] *)
val is_comment      : Parser_python.token -> bool
(*e: signature [[Token_helpers_python.is_comment]] *)

(*s: signature [[Token_helpers_python.info_of_tok]] *)
val info_of_tok : 
  Parser_python.token -> Parse_info.t
(*e: signature [[Token_helpers_python.info_of_tok]] *)
(*s: signature [[Token_helpers_python.visitor_info_of_tok]] *)
val visitor_info_of_tok : 
  (Parse_info.t -> Parse_info.t) -> Parser_python.token -> Parser_python.token
(*e: signature [[Token_helpers_python.visitor_info_of_tok]] *)
(*e: pfff/lang_python/parsing/Token_helpers_python.mli *)
