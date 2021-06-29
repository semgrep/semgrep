(* may raise AST_generic.Error *)
val program : Ast_js.a_program -> AST_generic.program

val any : Ast_js.any -> AST_generic.any

(* for lang_json/ *)
val expr : Ast_js.expr -> AST_generic.expr

(* for Parse_typescript_tree_sitter.ml *)
val parameter : Ast_js.parameter_classic -> AST_generic.parameter
