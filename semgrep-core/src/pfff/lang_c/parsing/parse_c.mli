
(* take care! this use Common.gensym to generate fresh unique anon structures
 * so this function may return a different program given the same input
*)
val parse:
  Common.filename ->
  (Ast_c.program, Parser_cpp.token) Parse_info.parsing_result

val parse_program:
  Common.filename -> Ast_c.program

(* other parsers *)

(* for sgrep *)
val any_of_string:
  string -> Ast_c.any
