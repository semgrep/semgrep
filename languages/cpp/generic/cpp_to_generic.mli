(* may raise AST_generic.Error *)
val program : Ast_cpp.program -> AST_generic.program

(* refer to `Rule_options.atd` for more details. *)
type cpp_parsing_option = [ `AsFunDef | `AsVarDefWithCtor ]

val any : ?parsing_opt:cpp_parsing_option -> Ast_cpp.any -> AST_generic.any
