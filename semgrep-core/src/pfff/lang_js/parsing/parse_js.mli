
(* This is the main function. It may raise
 *  - Parse_info.Parsing_error if Flag_parsing.error_recovery is false
 *  - Parse_info.Lexical_error if Flag_parsing.exn_when_lexical_error is true.
*)
val parse:
  ?timeout: float ->
  Common.filename ->
  (Ast_js.a_program, Parser_js.token) Parse_info.parsing_result

val parse_program:
  Common.filename -> Ast_js.a_program
val parse_string :
  string -> Ast_js.a_program

(* other parsers *)

(* for semgrep *)
val any_of_string:
  string -> Ast_js.any

(* for lsif *)
val type_of_string:
  string -> Ast_js.type_

(* to help write test code *)
val program_of_string: string -> Ast_js.a_program

(* internal *)
val tokens: Common.filename -> Parser_js.token list
