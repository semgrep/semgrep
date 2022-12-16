
val is_eof          : Parser_csharp.token -> bool
val is_comment      : Parser_csharp.token -> bool

val info_of_tok :
  Parser_csharp.token -> Parse_info.t
val visitor_info_of_tok :
  (Parse_info.t -> Parse_info.t) ->
  Parser_csharp.token -> Parser_csharp.token

val line_of_tok  : Parser_csharp.token -> int
val str_of_tok   : Parser_csharp.token -> string
val file_of_tok  : Parser_csharp.token -> Common.filename
val pos_of_tok   : Parser_csharp.token -> int
