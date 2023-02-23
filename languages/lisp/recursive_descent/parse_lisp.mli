(* the token list contains also the comment-tokens *)
type program_and_tokens = Ast_lisp.program option * Parser_lisp.token list

(* This is the main function *)
val parse : Common.filename -> program_and_tokens * Parsing_stat.t
val parse_program : Common.filename -> Ast_lisp.program

(* internal *)
val tokens : Parsing_helpers.input_stream -> Parser_lisp.token list
