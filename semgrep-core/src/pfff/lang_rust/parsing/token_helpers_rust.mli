
val is_eof          : Parser_rust.token -> bool
val is_comment      : Parser_rust.token -> bool

val info_of_tok :
  Parser_rust.token -> Parse_info.t
val visitor_info_of_tok :
  (Parse_info.t -> Parse_info.t) ->
  Parser_rust.token -> Parser_rust.token

val line_of_tok  : Parser_rust.token -> int
val str_of_tok   : Parser_rust.token -> string
val file_of_tok  : Parser_rust.token -> Common.filename
val pos_of_tok   : Parser_rust.token -> int
