(* This is the main function. It uses _defs below which often comes
 * from a standard.h macro file. It will raise Parse_error unless
 * Flag_parsing_cpp.error_recovery is set.
 *)
val parse : Fpath.t -> (Ast_cpp.program, Parser_cpp.token) Parsing_result.t
val parse_program : Fpath.t -> Ast_cpp.program

val parse_with_lang :
  ?lang:Flag_parsing_cpp.language ->
  Fpath.t ->
  (Ast_cpp.program, Parser_cpp.token) Parsing_result.t

(* other parsers *)
val any_of_string : Flag_parsing_cpp.language -> string -> Ast_cpp.any

val parse_fuzzy :
  Fpath.t -> Ast_fuzzy.trees * (Lib_ast_fuzzy.token_kind * Tok.t) list

(* usually correspond to what is inside your macros.h *)
val _defs : (string, Pp_token.define_body) Hashtbl.t
val init_defs : Fpath.t -> unit
val add_defs : Fpath.t -> unit

(* used to extract macros from standard.h, but also now used on C files
 * in -extract_macros to assist in building a macros.h
 *)
val extract_macros : Fpath.t -> (string, Pp_token.define_body) Assoc.t

(* usually correspond to what is inside your standard.h *)
(* val _defs_builtins : (string, Cpp_token_c.define_def) Hashtbl.t ref *)
(* todo: init_defs_macros and init_defs_builtins *)

(* subsystem testing *)
val tokens : Parsing_helpers.input_source -> Parser_cpp.token list
