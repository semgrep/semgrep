
(* This is the main function *)
val parse: Common.filename ->
  (Ast_java.program, Parser_java.token) Parse_info.parsing_result

(* may raise (Failure "just:pb") when parse return None when have parse error*)
val parse_program:
  Common.filename -> Ast_java.program

val parse_string:
  string -> (Ast_java.program, Parser_java.token) Parse_info.parsing_result

(* for sgrep *)
val any_of_string: string -> Ast_java.any


(* internal *)
val tokens: Common.filename -> Parser_java.token list
