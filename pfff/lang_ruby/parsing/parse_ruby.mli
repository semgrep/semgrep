(* the token list contains also the comment-tokens *)
type program_and_tokens = 
  Ast_ruby.program option * Parser_ruby.token list

val parse: Common.filename -> (program_and_tokens * Parse_info.parsing_stat)

val parse_program : Common.filename -> Ast_ruby.program

(* for semgrep *)
val any_of_string:
  string -> Ast_ruby.any
