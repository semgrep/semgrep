(* Returns the testsuite for dataflow analyses. To be concatenated by
 * the caller (e.g. in pfff/main_test.ml ) with other testsuites and
 * run via OUnit.run_test_tt
*)
val unittest: OUnit.test
