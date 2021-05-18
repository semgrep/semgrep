(*s: semgrep/matching/Generic_vs_generic.mli *)

(* entry points, used in the sgrep_generic visitors *)
(*s: signature [[Generic_vs_generic.m_expr]] *)
val m_expr : AST_generic.expr Matching_generic.matcher

(*e: signature [[Generic_vs_generic.m_expr]] *)
(*s: signature [[Generic_vs_generic.m_stmt]] *)
val m_stmt : AST_generic.stmt Matching_generic.matcher

(*e: signature [[Generic_vs_generic.m_stmt]] *)
(*s: signature [[Generic_vs_generic.m_stmts_deep]] *)
val m_stmts_deep :
  less_is_ok:bool -> AST_generic.stmt list Matching_generic.matcher

(*e: signature [[Generic_vs_generic.m_stmts_deep]] *)

val m_type_ : AST_generic.type_ Matching_generic.matcher

val m_pattern : AST_generic.pattern Matching_generic.matcher

val m_attribute : AST_generic.attribute Matching_generic.matcher

val m_partial : AST_generic.partial Matching_generic.matcher

val m_field : AST_generic.field Matching_generic.matcher

(*s: signature [[Generic_vs_generic.m_any]] *)
(* used only for unit testing *)
val m_any : AST_generic.any Matching_generic.matcher

(*e: signature [[Generic_vs_generic.m_any]] *)
(*e: semgrep/matching/Generic_vs_generic.mli *)
