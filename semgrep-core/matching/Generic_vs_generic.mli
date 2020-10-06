(*s: semgrep/matching/Generic_vs_generic.mli *)

(* entry points, used in the sgrep_generic visitors *)
(*s: signature [[Generic_vs_generic.m_expr]] *)
val m_expr : (AST.expr, AST.expr) Matching_generic.matcher
(*e: signature [[Generic_vs_generic.m_expr]] *)
(*s: signature [[Generic_vs_generic.m_stmt]] *)
val m_stmt : (AST.stmt, AST.stmt) Matching_generic.matcher
(*e: signature [[Generic_vs_generic.m_stmt]] *)
(*s: signature [[Generic_vs_generic.m_stmts_deep]] *)
val m_stmts_deep : less_is_ok:bool->
  (AST.stmt list, AST.stmt list) Matching_generic.matcher
(*e: signature [[Generic_vs_generic.m_stmts_deep]] *)

val m_type_ :   (AST_generic.type_, AST_generic.type_) Matching_generic.matcher
val m_pattern : (AST_generic.pattern, AST_generic.pattern) Matching_generic.matcher
val m_partial : (AST_generic.partial, AST_generic.partial) Matching_generic.matcher

(*s: signature [[Generic_vs_generic.m_any]] *)
(* used only for unit testing *)
val m_any : (AST.any, AST.any) Matching_generic.matcher
(*e: signature [[Generic_vs_generic.m_any]] *)
(*e: semgrep/matching/Generic_vs_generic.mli *)
