(* Returns the testsuite for dataflow analyses. To be concatenated by
 * the caller (e.g. in pfff/main_test.ml ) with other testsuites and
 * run via Alcotest.run.
 *)
val tests : (Common.filename -> AST_generic.program) -> Testutil.test list
