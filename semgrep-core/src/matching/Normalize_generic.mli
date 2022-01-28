val normalize_import_opt :
  bool ->
  AST_generic.directive_kind ->
  (AST_generic.tok * AST_generic.module_name) option

(* you can extend the behavior of this function by setting
 * hook_constant_propagation_and_evaluate_literal below.
 *)
val constant_propagation_and_evaluate_literal :
  AST_generic.expr -> AST_generic.svalue option

val hook_constant_propagation_and_evaluate_literal :
  (AST_generic.expr -> AST_generic.svalue option) option ref
