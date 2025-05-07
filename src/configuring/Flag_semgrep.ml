(* !!! DEPRECATED !!!

   1. Do not add new globals refs to this module or anywhere else.
      Pass parameters to functions.
   2. Try to reduce the size of this module when you have a chance.
*)

(* Debugging flags

   Debugging flags are mostly ok.
*)

(* To not capture exn and instead let them bubble up to get a precise
 * backtrace when there is an error. This is useful for debugging.
 *)
let fail_fast = Hook.create false

(* note that this will stop at the first fail(), but if you restrict
 * enough your pattern, this can help you debug your problem.*)
let debug_matching = Hook.create false

(* we usually try first with the pfff parser and then with the tree-sitter
 * parser if pfff fails. Here you can force to only use tree-sitter.
 *)
let tree_sitter_only = Hook.create false
let pfff_only = Hook.create false

(* Optimization flags - DEPRECATED
   TODO: pack these into the big configuration record where they belong
*)

(* look if identifiers in pattern intersect with file using simple regexps *)
let filter_irrelevant_patterns = Hook.create false

(* Maximum number of tainted lvals to save. *)
let max_tainted_vars = ref Limits_semgrep.taint_MAX_TAINTED_VARS

(* Maximum size of the taints set for each lval *)
let max_taint_set_size = ref Limits_semgrep.taint_MAX_TAINT_SET_SIZE

(* Note that an important flag used during parsing is actually in pfff in
 * Flag_parsing.sgrep_mode
 *)
