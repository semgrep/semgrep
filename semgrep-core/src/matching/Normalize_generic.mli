(*s: semgrep/matching/Normalize_generic.mli *)

(*s: signature [[Normalize_generic.normalize_import_opt]] *)
val normalize_import_opt :
  bool ->
  AST_generic.directive ->
  (AST_generic.tok * AST_generic.module_name) option

(*e: signature [[Normalize_generic.normalize_import_opt]] *)

(*s: signature [[Normalize_generic.constant_propagation_and_evaluate_literal]] *)
val constant_propagation_and_evaluate_literal :
  AST_generic.expr -> AST_generic.constness option

(*e: signature [[Normalize_generic.constant_propagation_and_evaluate_literal]] *)
(*e: semgrep/matching/Normalize_generic.mli *)
