
val find_source_files_of_dir_or_files:
  Common.path list -> Common.filename list


(* This is the main function. See flag_parsing for settings. *)
val parse:
  Common.filename ->
  (AST_scala.program, Parser_scala.token) Parse_info.parsing_result

val parse_program:
  Common.filename -> AST_scala.program

(* for semgrep *)
val any_of_string: string -> AST_scala.any

(* internal *)
val tokens: Common.filename -> Parser_scala.token list
