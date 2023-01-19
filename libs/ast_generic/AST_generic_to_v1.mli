exception SemgrepConstruct of Parse_info.t

(* may raise SemgrepConstruct *)
val program : AST_generic.program -> Ast_generic_v1_t.program
