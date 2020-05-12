(*s: semgrep/matching/generic_vs_generic.mli *)

(* entry points, used in the sgrep_generic visitors *)
(*s: signature [[Generic_vs_generic.m_expr]] *)
val m_expr : (Ast.expr, Ast.expr) Matching_generic.matcher
(*e: signature [[Generic_vs_generic.m_expr]] *)
(*s: signature [[Generic_vs_generic.m_stmt]] *)
val m_stmt : (Ast.stmt, Ast.stmt) Matching_generic.matcher
(*e: signature [[Generic_vs_generic.m_stmt]] *)
(*s: signature [[Generic_vs_generic.m_stmts_deep]] *)
val m_stmts_deep : (Ast.stmt list, Ast.stmt list) Matching_generic.matcher
(*e: signature [[Generic_vs_generic.m_stmts_deep]] *)

(*s: signature [[Generic_vs_generic.m_any]] *)
(* used only for unit testing *)
val m_any : (Ast.any, Ast.any) Matching_generic.matcher
(*e: signature [[Generic_vs_generic.m_any]] *)
(*e: semgrep/matching/generic_vs_generic.mli *)
