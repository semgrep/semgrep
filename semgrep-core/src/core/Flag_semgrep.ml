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

(* similar to filter_irrelevant_patterns, but use the whole rule to extract
 * the regexp *)
let filter_irrelevant_rules = ref false

(* check for identifiers before attempting to match a stmt or stmt list *)
let use_bloom_filter = ref true

(* opt = optimization *)
let with_opt_cache = ref true

(* Improves performance on some patterns, degrades performance on others. *)
let max_cache = ref false

(* Maximum size of a single target file, in bytes (exceptions apply). *)
let max_target_bytes = ref 5_000_000

(* Disabling this lets us measure the effectiveness of our GC tuning. *)
let gc_tuning = ref true

(* special mode to set before using generic_vs_generic to match
 * code equivalences.
 *)
let equivalence_mode = ref false

(* Note that an important flag used during parsing is actually in pfff in
 * Flag_parsing.sgrep_mode
 *)
