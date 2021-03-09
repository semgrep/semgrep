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


(* look if identifiers in pattern intersect with file using simple regexps *)
let filter_irrelevant_patterns = ref false

(* similar to filter_irrelevant_patterns, but use the whole rule to extract
 * the regexp *)
let filter_irrelevant_rules = ref false

(* check for identifiers before attempting to match a stmt or stmt list *)
let use_bloom_filter = ref false

(* opt = optimization *)
let with_opt_cache = ref true
(* Improves performance on some patterns, degrades performance on others. *)
let max_cache = ref false

(* we usually try first with the pfff parser and then with the tree-sitter
 * parser if pfff fails. Here you can force to only use tree-sitter.
*)
let tree_sitter_only = ref false
let pfff_only = ref false

(*s: constant [[Flag_semgrep.equivalence_mode]] *)
(* special mode to set before using generic_vs_generic to match
 * code equivalences.
*)
let equivalence_mode = ref false
(*e: constant [[Flag_semgrep.equivalence_mode]] *)

(*e: semgrep/core/Flag_semgrep.ml *)
