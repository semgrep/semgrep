
type program_and_tokens =
  Ast_rust.program * Parser_rust.token list

(* This is the main function *)
val parse:
  Common.filename -> (program_and_tokens * Parse_info.parsing_stat)

(* internal *)
val tokens: Common.filename -> Parser_rust.token list
