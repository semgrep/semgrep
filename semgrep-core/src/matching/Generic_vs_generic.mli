(* entry points, used in the sgrep_generic visitors *)
val m_expr_root : AST_generic.expr Matching_generic.matcher
val m_stmt : AST_generic.stmt Matching_generic.matcher

val m_stmts_deep :
  inside:bool ->
  less_is_ok:bool ->
  AST_generic.stmt list Matching_generic.matcher

val m_type_ : AST_generic.type_ Matching_generic.matcher
val m_pattern : AST_generic.pattern Matching_generic.matcher
val m_attribute : AST_generic.attribute Matching_generic.matcher
val m_partial : AST_generic.partial Matching_generic.matcher
val m_field : AST_generic.field Matching_generic.matcher
val m_fields : AST_generic.field list Matching_generic.matcher

(* used only for unit testing *)
val m_any : AST_generic.any Matching_generic.matcher

val hook_find_possible_parents :
  (AST_generic.dotted_ident -> AST_generic.name list) option ref
