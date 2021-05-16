(*s: semgrep/matching/SubAST_generic.mli *)

(*s: signature [[SubAST_generic.subexprs_of_expr]] *)
val subexprs_of_expr : AST_generic.expr -> AST_generic.expr list

(*e: signature [[SubAST_generic.subexprs_of_expr]] *)

(*s: signature [[SubAST_generic.flatten_substmts_of_stmts]] *)
val flatten_substmts_of_stmts :
  AST_generic.stmt list -> (AST_generic.stmt list * AST_generic.stmt) option

(*e: signature [[SubAST_generic.flatten_substmts_of_stmts]] *)
(*e: semgrep/matching/SubAST_generic.mli *)
