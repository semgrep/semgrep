val find_source_files_of_dir_or_files : Fpath.t list -> Fpath.t list

(* This is the main function. See flag_parsing for settings. *)
val parse : Fpath.t -> (AST_scala.program, Parser_scala.token) Parsing_result.t
val parse_program : Fpath.t -> AST_scala.program

(* for semgrep *)
val any_of_string : string -> AST_scala.any

(* internal *)
val tokens : Parsing_helpers.input_source -> Parser_scala.token list
