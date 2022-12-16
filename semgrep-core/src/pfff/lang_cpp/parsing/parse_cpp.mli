
(* This is the main function. It uses _defs below which often comes
 * from a standard.h macro file. It will raise Parse_error unless
 * Flag_parsing_cpp.error_recovery is set.
*)
val parse:
  Common.filename ->
  (Ast_cpp.program, Parser_cpp.token) Parse_info.parsing_result

val parse_program:
  Common.filename -> Ast_cpp.program
val parse_with_lang:
  ?lang:Flag_parsing_cpp.language ->
  Common.filename ->
  (Ast_cpp.program, Parser_cpp.token) Parse_info.parsing_result


(* other parsers *)
val any_of_string:
  Flag_parsing_cpp.language -> string -> Ast_cpp.any

val parse_fuzzy:
  Common.filename ->
  Ast_fuzzy.trees * (Parse_info.token_kind * Parse_info.t) list

(* now in old_glr/
   val parse_with_dypgen:
   Common.filename -> Ast_cpp.program
*)

(* usually correspond to what is inside your macros.h *)
val _defs : (string, Pp_token.define_body) Hashtbl.t
val init_defs : Common.filename -> unit
val add_defs : Common.filename -> unit
(* used to extract macros from standard.h, but also now used on C files
 * in -extract_macros to assist in building a macros.h
*)
val extract_macros:
  Common.filename -> (string, Pp_token.define_body) Common.assoc

(* usually correspond to what is inside your standard.h *)
(* val _defs_builtins : (string, Cpp_token_c.define_def) Hashtbl.t ref *)
(* todo: init_defs_macros and init_defs_builtins *)

(* subsystem testing *)
val tokens:      Common.filename -> Parser_cpp.token list
