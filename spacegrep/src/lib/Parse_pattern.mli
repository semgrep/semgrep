
val of_lexbuf: ?is_doc:bool -> Lexing.lexbuf -> Pattern_AST.t

val of_src: ?is_doc:bool -> Src_file.t -> Pattern_AST.t
