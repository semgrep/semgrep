(* may raise AST_generic.Error *)
val program : Ast_cpp.program -> AST_generic.program

val any :
  Rule_options_t.cpp_parsing_opt option -> Ast_cpp.any -> AST_generic.any
