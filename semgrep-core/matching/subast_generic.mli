(*s: semgrep/matching/subast_generic.mli *)

(*s: signature [[Subast_generic.subexprs_of_expr]] *)
val subexprs_of_expr: Ast.expr -> Ast.expr list
(*e: signature [[Subast_generic.subexprs_of_expr]] *)

(*s: signature [[Subast_generic.flatten_substmts_of_stmts]] *)
val flatten_substmts_of_stmts: Ast.stmt list -> Ast.stmt list
(*e: signature [[Subast_generic.flatten_substmts_of_stmts]] *)
(*e: semgrep/matching/subast_generic.mli *)
