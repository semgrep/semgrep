(* debugging flags *)

(* To not capture exn and instead let them bubble up to get a precise
 * backtrace when there is an error. This is useful for debugging.
 *)
let fail_fast = ref false

(* note that this will stop at the first fail(), but if you restrict
 * enough your pattern, this can help you debug your problem.*)
let debug_matching = ref false

(* we usually try first with the pfff parser and then with the tree-sitter
 * parser if pfff fails. Here you can force to only use tree-sitter.
 *)
let tree_sitter_only = ref false
let pfff_only = ref false

(* optimization flags *)

(* look if identifiers in pattern intersect with file using simple regexps *)
let filter_irrelevant_patterns = ref false

(* TODO: This was turned off by default in 1.22.0, the matching-cache's code
 * should be removed after a few releases once we confirm that disabling it
 * has no major impact on performance for our community users and customers. *)
(* opt = optimization *)
let with_opt_cache = ref false

(* TODO: To be removed with `with_opt_cache` a few releases after 1.22.0. *)
(* Improves performance on some patterns, degrades performance on others. *)
let max_cache = ref false

(* Maximum size of a single target file, in bytes (exceptions apply). *)
let max_target_bytes = ref 5_000_000

(* Maximum number of tainted lvals to save. *)
let max_tainted_lvals = ref Limits_semgrep.taint_MAX_TAINTED_LVALS

(* Maximum size of the taints set for each lval *)
let max_taint_set_size = ref Limits_semgrep.taint_MAX_TAINT_SET_SIZE

(* Whether or not to skip files believed to be minified. *)
let skip_minified_files = ref true

(* Disabling this lets us measure the effectiveness of our GC tuning. *)
let gc_tuning = ref true

(* special mode to set before using generic_vs_generic to match
 * code equivalences.
 *)
let equivalence_mode = ref false

(* Note that an important flag used during parsing is actually in pfff in
 * Flag_parsing.sgrep_mode
 *)

(* One-off experiment for Raja (See Raja_experiment.ml) *)
let raja = ref false
