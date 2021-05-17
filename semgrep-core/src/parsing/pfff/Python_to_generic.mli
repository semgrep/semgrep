(*s: pfff/lang_python/analyze/Python_to_generic.mli *)

(*s: signature [[Python_to_generic.program]] *)
val program : AST_python.program -> AST_generic.program

(*e: signature [[Python_to_generic.program]] *)

(*s: signature [[Python_to_generic.any]] *)
val any : AST_python.any -> AST_generic.any

(*e: signature [[Python_to_generic.any]] *)

(* exception Error of string * Parse_info.info *)
(* may raise Error *)
(*e: pfff/lang_python/analyze/Python_to_generic.mli *)
