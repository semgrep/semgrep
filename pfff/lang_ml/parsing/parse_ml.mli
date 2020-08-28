
(* the token list contains also the comment-tokens *)
type program_and_tokens = 
  Cst_ml.program option * Parser_ml.token list

(* This is the main function. See flag_parsing_ml for settings. *)
val parse:
  Common.filename -> (program_and_tokens * Parse_info.parsing_stat)

val parse_program:
  Common.filename -> Cst_ml.program

(* for semgrep *)
val any_of_string:
  string -> Cst_ml.any

(* internal *)
val tokens: Common.filename -> Parser_ml.token list
