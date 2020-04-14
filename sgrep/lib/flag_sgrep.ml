let verbose = ref false

(* note that this will stop at the first fail(), but if you restrict
 * enough your pattern, this can help you debug your problem.*)
let debug = ref false
let debug_with_full_position = ref false

(* !experimental: a bit hacky, and may introduce big perf regressions! *)

(* should be used with DeepEllipsis; do it implicitely has issues *)
let go_deeper_expr = ref true
(* this ultimately should go away once '...' works on the CFG *)
let go_deeper_stmt = ref true
(* not sure we want that ... *)
let go_really_deeper_stmt = ref true

(* special mode to set before using generic_vs_generic to match
 * code equivalences.
 *)
let equivalence_mode = ref false
