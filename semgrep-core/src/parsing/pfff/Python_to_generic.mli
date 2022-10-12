val program :
  ?assign_to_vardef:bool -> AST_python.program -> AST_generic.program

val any : AST_python.any -> AST_generic.any
val type_for_lsif : AST_python.type_ -> AST_generic.type_

(* exception Error of string * Parse_info.info *)
(* may raise Error *)
