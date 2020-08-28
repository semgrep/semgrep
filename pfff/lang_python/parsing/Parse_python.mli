(*s: pfff/lang_python/parsing/Parse_python.mli *)

(*s: type [[Parse_python.program_and_tokens]] *)
(* the token list contains also the comment-tokens *)
type program_and_tokens = 
  AST_python.program option * Parser_python.token list
(*e: type [[Parse_python.program_and_tokens]] *)

(*s: type [[Parse_python.parsing_mode]] *)
type parsing_mode =
  | Python2
  | Python3
  (* will start with Python3 and fallback to Python2 in case of an error *)
  | Python
(*e: type [[Parse_python.parsing_mode]] *)

(*s: signature [[Parse_python.parse]] *)
(* This is the main function.
 * can throw Parse_info.Lexical_error and Parse_info.Parsing_error *)
val parse:
  ?parsing_mode:parsing_mode (* default mode is Python *) ->
  Common.filename -> (program_and_tokens * Parse_info.parsing_stat)
(*e: signature [[Parse_python.parse]] *)

(*s: signature [[Parse_python.parse_program]] *)
val parse_program:
  ?parsing_mode:parsing_mode ->
  Common.filename -> AST_python.program
(*e: signature [[Parse_python.parse_program]] *)

(* other parsers *)

(*s: signature [[Parse_python.any_of_string]] *)
(* for semgrep *)
val any_of_string:
  ?parsing_mode:parsing_mode -> string -> AST_python.any
(*e: signature [[Parse_python.any_of_string]] *)

(* for sgrep via fuzzy AST *)
(*
val parse_fuzzy:
  Common.filename -> Ast_fuzzy.trees * Parser_python.token list
*)

(*s: signature [[Parse_python.program_of_string]] *)
(* to help write test code *)
val program_of_string: string -> AST_python.program
(*e: signature [[Parse_python.program_of_string]] *)


(*s: signature [[Parse_python.tokens]] *)
(* internal *)
val tokens: parsing_mode -> Common.filename -> Parser_python.token list
(*e: signature [[Parse_python.tokens]] *)
(*e: pfff/lang_python/parsing/Parse_python.mli *)
