(* This is the main function. raise Parse_error when not Flag.error_recovery.*)
val parse : Fpath.t -> (Cst_php.program, Parser_php.token) Parsing_result.t
val parse_program : Fpath.t -> Cst_php.program

(* for sgrep/spatch patterns *)
val any_of_string : string -> Cst_php.any

val tokens :
  ?init_state:Lexer_php.state_mode ->
  Parsing_helpers.input_source ->
  Parser_php.token list

(* useful in tests *)
val program_of_string : string -> Cst_php.program
