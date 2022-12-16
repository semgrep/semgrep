(*
   Parse a full program.
   There's no timeout by default.
*)
val parse:
  ?timeout: float ->
  Common.filename ->
  (Ast_ruby.program, Parser_ruby.token) Parse_info.parsing_result

val parse_program : Common.filename -> Ast_ruby.program

(*
   For parsing semgrep patterns.
   There's no timeout by default.
*)
val any_of_string:
  ?timeout: float ->
  string -> Ast_ruby.any
