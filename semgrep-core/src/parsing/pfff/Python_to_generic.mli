val program :
  ?assign_to_vardef:bool -> AST_python.program -> AST_generic.program

val any : AST_python.any -> AST_generic.any

(* exception Error of string * Parse_info.info *)
(* may raise Error *)
