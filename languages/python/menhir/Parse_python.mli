type parsing_mode =
  | Python2
  | Python3
  (* will start with Python3 and fallback to Python2 in case of an error *)
  | Python

(* This is the main function.
 * can throw Parse_info.Lexical_error and Parse_info.Parsing_error.
 * The token list in parsing_result contains also the comment-tokens.
 *)
val parse :
  ?parsing_mode:parsing_mode (* default mode is Python *) ->
  Fpath.t ->
  (AST_python.program, Parser_python.token) Parsing_result.t

val parse_program : ?parsing_mode:parsing_mode -> Fpath.t -> AST_python.program

(* other parsers *)

(* for semgrep *)
val any_of_string : ?parsing_mode:parsing_mode -> string -> AST_python.any

(* for lsif *)
val type_of_string :
  ?parsing_mode:parsing_mode -> string -> AST_python.lsif_type

(* for sgrep via fuzzy AST *)
(*
val parse_fuzzy:
  string (* filename *) -> Ast_fuzzy.trees * Parser_python.token list
*)

(* to help write test code *)
val program_of_string : < Cap.tmp > -> string -> AST_python.program

(* internal *)
val tokens :
  parsing_mode -> Parsing_helpers.input_source -> Parser_python.token list
