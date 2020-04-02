
(* entry points, used in the sgrep_generic visitors *)
val m_expr : (Ast.expr, Ast.expr) Matching_generic.matcher
val m_stmt : (Ast.stmt, Ast.stmt) Matching_generic.matcher
val m_stmts_deep : (Ast.stmt list, Ast.stmt list) Matching_generic.matcher

(* used only for unit testing *)
val m_any : (Ast.any, Ast.any) Matching_generic.matcher
