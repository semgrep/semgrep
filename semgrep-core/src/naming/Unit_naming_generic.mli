(* Returns the test suite for the current directory. To be concatenated by
 * the caller (e.g. in pfff/main_test.ml ) with other test suites.
 *)
val tests : (Common.filename -> AST_generic.program) -> Testutil.test list
