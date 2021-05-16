(* Returns the testsuite for matcher/. To be concatenated by
 * the caller (e.g. in pfff/main_test.ml ) with other testsuites and
 * run via OUnit.run_test_tt
 *)
val unittest : OUnit.test

(* subsystems unittest *)
val sgrep_unittest : OUnit.test

val spatch_unittest : OUnit.test

val refactoring_unittest : OUnit.test
