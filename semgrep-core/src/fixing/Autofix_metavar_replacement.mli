(* Replace metavars appearing in the given AST with their bound values, if
 * possible *)
val replace_metavars :
  Metavariable.bindings -> AST_generic.any -> (AST_generic.any, string) result
