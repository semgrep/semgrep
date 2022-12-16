
(* may raise Parse_info.Parsing_error exn *)
val parse: Parser_scala.token list -> AST_scala.program

val semgrep_pattern: Parser_scala.token list -> AST_scala.any
