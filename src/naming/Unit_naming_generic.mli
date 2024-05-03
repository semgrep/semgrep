(* Returns the test suite for the current directory. To be concatenated by
 * the caller (e.g. in Test.ml) with other test suites.
 *)
val tests : (Fpath.t -> AST_generic.program) -> Testo.t list
