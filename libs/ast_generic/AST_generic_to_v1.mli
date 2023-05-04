exception SemgrepConstruct of Tok.t

(* may raise SemgrepConstruct *)
val program : AST_generic.program -> Ast_generic_v1_t.program
