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

(* check for identifiers before attempting to match a stmt or stmt list
 *
 * We disabled the Bloom filter optimization by default in 0.116.0 due to its
 * bad interaction with const-prop and sym-prop, and because nowadays it appears
 * to only have a marginal benefit on performance. If we don't notice any major
 * perf regressions, we could completely drop the optimization in the future.
 *
 * For an example of how it interacts badly with const-prop see GH #4670, and for
 * an example of how it interacts badly with sym-prop see PA-1920 (or GH PR #6179).
 * Regarding performance, we ran Semgrep with and without this optimization on
 * nine of the repos in our stress-test monorepo (using p/default):
 *
 * - In four cases (elasticsearch, maven, metabase, and kubernetes) using the
 *   Bloom filter was about 4-5% slower.
 * - In four cases (hadoop, mypy, react, mediawiki) using the Bloom filter was an
 *   average of 9.5% faster, with the largest effect seen on mypy (15% faster).
 * - In the remaining case (flask) there was no meaningful difference.
 *
 * That said take these %'s with a grain of salt, since we only did one run each,
 * and it was done on a laptop with other stuff running on the bakground, so there
 * was noise. And the absolute difference in seconds between runs was less than
 * 8 seconds, with an average (and also median) of 4 seconds.
 *)
let use_bloom_filter = ref false

(* opt = optimization *)
let with_opt_cache = ref true

(* Improves performance on some patterns, degrades performance on others. *)
let max_cache = ref false

(* Maximum size of a single target file, in bytes (exceptions apply). *)
let max_target_bytes = ref 5_000_000

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
