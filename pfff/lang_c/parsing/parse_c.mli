
(* the token list contains also the comment-tokens *)
type program_and_tokens = 
  Ast_c.program option * Parser_cpp.token list

(* take care! this use Common.gensym to generate fresh unique anon structures
 * so this function may return a different program given the same input
 *)
val parse: 
  Common.filename -> (program_and_tokens * Parse_info.parsing_stat)

val parse_program:
  Common.filename -> Ast_c.program

(* other parsers *)

(* for sgrep *)
val any_of_string:
  string -> Ast_c.any
