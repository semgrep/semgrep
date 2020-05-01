(*s: semgrep/matching/normalize_generic.mli *)

(*s: signature [[Normalize_generic.normalize_import_opt]] *)
val normalize_import_opt: 
  bool -> Ast_generic.directive -> 
  (Ast_generic.tok * Ast_generic.module_name) option
(*e: signature [[Normalize_generic.normalize_import_opt]] *)

(*s: signature [[Normalize_generic.constant_propagation_and_evaluate_literal]] *)
val constant_propagation_and_evaluate_literal:
  Ast_generic.expr -> Ast_generic.literal option
(*e: signature [[Normalize_generic.constant_propagation_and_evaluate_literal]] *)
(*e: semgrep/matching/normalize_generic.mli *)
