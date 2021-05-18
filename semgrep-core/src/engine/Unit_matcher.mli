(*s: semgrep/engine/Unit_matcher.mli *)

(*s: signature [[Unit_matcher.unittest]] *)
(* Returns the testsuite for this directory To be concatenated by
 * the caller (e.g. in pfff/main_test.ml ) with other testsuites and
 * run via OUnit.run_test_tt
 *)
val unittest : any_gen_of_string:(string -> AST_generic.any) -> OUnit.test

(*e: signature [[Unit_matcher.unittest]] *)
(*e: semgrep/engine/Unit_matcher.mli *)
