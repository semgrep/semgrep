(* take care! this use Common.gensym to generate fresh unique anon structures
 * so this function may return a different program given the same input
 *)
val parse : Fpath.t -> (Ast_c.program, Parser_cpp.token) Parsing_result.t
val parse_program : Fpath.t -> Ast_c.program

(* other parsers *)

(* for sgrep *)
val any_of_string : string -> Ast_c.any
