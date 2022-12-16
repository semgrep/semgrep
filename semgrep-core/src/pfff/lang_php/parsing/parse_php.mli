
(* This is the main function. raise Parse_error when not Flag.error_recovery.*)
val parse :
  ?pp:string option ->
  Common.filename ->
  (Cst_php.program, Parser_php.token) Parse_info.parsing_result

val parse_program:
  ?pp:string option ->
  Common.filename -> Cst_php.program

(* for sgrep/spatch patterns *)
val any_of_string:  string -> Cst_php.any

val xdebug_expr_of_string: string -> Cst_php.expr
val expr_of_string: string -> Cst_php.expr
val program_of_string: string -> Cst_php.program
val tokens_of_string: string -> Parser_php.token list

val tmp_php_file_from_string: ?header:string -> string -> Common.filename
val tokens:
  ?init_state:Lexer_php.state_mode ->
  Common.filename -> Parser_php.token list
