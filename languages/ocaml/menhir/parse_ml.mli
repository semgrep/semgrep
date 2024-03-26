(* This is the main function. See flag_parsing_ml for settings. *)
val parse : Fpath.t -> (AST_ocaml.program, Parser_ml.token) Parsing_result.t
val parse_program : Fpath.t -> AST_ocaml.program

(* for semgrep *)
val any_of_string : string -> AST_ocaml.any

(* for semgrep and LSP *)
val type_of_string : string -> AST_ocaml.type_

(* internal *)
val tokens : Parsing_helpers.input_source -> Parser_ml.token list
