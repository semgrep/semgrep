(* This is the main function *)
val parse :
  Common.filename -> (Ast_java.program, Parser_java.token) Parsing_result.t

(* may raise (Failure "just:pb") when parse return None when have parse error*)
val parse_program : Common.filename -> Ast_java.program

val parse_string :
  string -> (Ast_java.program, Parser_java.token) Parsing_result.t

(* for sgrep *)
val any_of_string : string -> Ast_java.any

(* internal *)
val tokens : Parsing_helpers.input_source -> Parser_java.token list
