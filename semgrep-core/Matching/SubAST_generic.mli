(*s: semgrep/matching/SubAST_generic.mli *)

(*s: signature [[SubAST_generic.subexprs_of_expr]] *)
val subexprs_of_expr: AST.expr -> AST.expr list
(*e: signature [[SubAST_generic.subexprs_of_expr]] *)

(*s: signature [[SubAST_generic.flatten_substmts_of_stmts]] *)
val flatten_substmts_of_stmts: AST.stmt list -> AST.stmt list option
(*e: signature [[SubAST_generic.flatten_substmts_of_stmts]] *)
(*e: semgrep/matching/SubAST_generic.mli *)
