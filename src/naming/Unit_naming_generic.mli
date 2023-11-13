(* Returns the test suite for the current directory. To be concatenated by
 * the caller (e.g. in Test.ml) with other test suites.
 *)
val tests : (string (* filename *) -> AST_generic.program) -> Testutil.test list
