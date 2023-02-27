(* This is the main function. See flag_parsing_ml for settings. *)
val parse :
  Common.filename -> (Ast_ml.program, Parser_ml.token) Parsing_result.t

val parse_program : Common.filename -> Ast_ml.program

(* for semgrep *)
val any_of_string : string -> Ast_ml.any

(* for semgrep and LSP *)
val type_of_string : string -> Ast_ml.type_

(* internal *)
val tokens : Parsing_helpers.input_source -> Parser_ml.token list
