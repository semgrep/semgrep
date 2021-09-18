val normalize_import_opt :
  bool ->
  AST_generic.directive_kind ->
  (AST_generic.tok * AST_generic.module_name) option

val constant_propagation_and_evaluate_literal :
  AST_generic.expr -> AST_generic.constness option
