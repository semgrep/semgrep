exception Syntax_error of string

(* can raise Syntax_error *)
val token : Lexing.lexbuf -> Parser.token
