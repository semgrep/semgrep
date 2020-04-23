
val normalize_import_opt: 
  bool -> Ast_generic.directive -> 
  (Ast_generic.tok * Ast_generic.module_name) option

val constant_propagation_and_evaluate_literal:
  Ast_generic.expr -> Ast_generic.literal option
