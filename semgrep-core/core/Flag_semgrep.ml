(*s: semgrep/core/Flag_semgrep.ml *)
(*s: constant [[Flag_semgrep.verbose]] *)
(*e: constant [[Flag_semgrep.verbose]] *)

(*s: constant [[Flag_semgrep.debug]] *)
(* note that this will stop at the first fail(), but if you restrict
 * enough your pattern, this can help you debug your problem.*)
let debug_matching = ref false
(*e: constant [[Flag_semgrep.debug]] *)
(*s: constant [[Flag_semgrep.debug_with_full_position]] *)
let debug_with_full_position = ref false
(*e: constant [[Flag_semgrep.debug_with_full_position]] *)

(* !experimental: a bit hacky, and may introduce big perf regressions! *)

(*s: constant [[Flag_semgrep.go_deeper_expr]] *)
(* should be used with DeepEllipsis; do it implicitely has issues *)
let go_deeper_expr = ref true
(*e: constant [[Flag_semgrep.go_deeper_expr]] *)
(*s: constant [[Flag_semgrep.go_deeper_stmt]] *)
(* this ultimately should go away once '...' works on the CFG *)
let go_deeper_stmt = ref true
(*e: constant [[Flag_semgrep.go_deeper_stmt]] *)
(*s: constant [[Flag_semgrep.go_really_deeper_stmt]] *)
(* not sure we want that ... *)
let go_really_deeper_stmt = ref true
(*e: constant [[Flag_semgrep.go_really_deeper_stmt]] *)

(* look if identifiers in rule intersect with file using simple regexps *)
let filter_irrelevant_rules = ref false

(* we usually try first with the pfff parser and then with the tree-sitter
 * parser if pfff fails. Here you can force to only use tree-sitter.
*)
let tree_sitter_only = ref false

(*s: constant [[Flag_semgrep.equivalence_mode]] *)
(* special mode to set before using generic_vs_generic to match
 * code equivalences.
*)
let equivalence_mode = ref false
(*e: constant [[Flag_semgrep.equivalence_mode]] *)

(* here and not in Main.ml because we need to know if the timeout was
 * set in Parse_code.ml (for the slow pfff-based Javascript parser) *)
let timeout = ref 0. (* in seconds *)

(*e: semgrep/core/Flag_semgrep.ml *)
