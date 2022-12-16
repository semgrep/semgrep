
val pr2_pp: string -> unit

val set_as_comment:
  Token_cpp.cppcommentkind -> Token_views_cpp.token_extended -> unit

val msg_context:
  Parser_cpp.token -> Token_views_cpp.context -> unit

val change_tok:
  Token_views_cpp.token_extended -> Parser_cpp.token -> unit
val fresh_tok:
  Parser_cpp.token -> Parser_cpp.token


val regexp_ns_decl_like: Str.regexp
val regexp_macro: Str.regexp
val regexp_declare: Str.regexp
