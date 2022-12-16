
type program_and_tokens =
  Ast_csharp.program (* NotParsedCorrectly if parse error *) *
  Parser_csharp.token list

(* This is the main function *)
val parse:
  Common.filename -> (program_and_tokens * Parse_info.parsing_stat)

(* internal *)
val tokens: Common.filename -> Parser_csharp.token list
