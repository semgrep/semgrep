open Cst_php

(*s: lib_parsing_php.mli *)
val is_php_filename: Common.filename -> bool

(* this will open the file and look possible for a #!/usr/bin/env php *)
val is_php_file: Common.filename -> bool
val is_php_script: Common.filename -> bool

val find_source_files_of_dir_or_files:
  ?verbose:bool ->
  ?include_hack:bool ->
  Common.path list -> Common.filename list

(*x: lib_parsing_php.mli *)
(*
(* returns only origin tokens, filter fake tokens *)
val ii_of_any: Cst_php.any -> Cst_php.tok list
*)
(*x: lib_parsing_php.mli *)

(*x: lib_parsing_php.mli *)
val range_of_origin_ii: tok list -> (int * int) option

(*x: lib_parsing_php.mli *)
(*
val get_funcalls_any         : any -> string list
val get_constant_strings_any : any -> string list
val get_vars_any              : any -> dname list
val get_static_vars_any       : any -> dname list
val get_returns_any           : any -> expr list
val get_vars_assignements_any : any -> (string * expr list) list

val top_statements_of_program:
  program -> stmt list

val functions_methods_or_topstms_of_program:
  program ->
  (func_def list * method_def list * stmt list list)
*)
(*e: lib_parsing_php.mli *)
