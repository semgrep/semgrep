
(* the token list contains also the comment-tokens *)
type program_and_tokens = Ast_hs.program * Parser_hs.token list

(* This is the main function *)
val parse:
  Common.filename -> (program_and_tokens * Parse_info.parsing_stat)


(* internal *)
val tokens: Common.filename -> Parser_hs.token list
