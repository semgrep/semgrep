
(* the token list contains also the comment-tokens *)
type program_and_tokens = Ast_nw.program * Lexer_nw.token list

(* This is the main function *)
val parse:
  Common.filename -> (program_and_tokens * Parse_info.parsing_stat)

val parse_fuzzy:
  Common.filename -> Ast_fuzzy.tree list * Lexer_nw.token list

(* internal *)
val tokens: Common.filename -> Lexer_nw.token list
