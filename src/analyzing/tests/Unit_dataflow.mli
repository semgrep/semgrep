(* Returns the testsuite for dataflow analyses. To be concatenated by
 * the caller (e.g. in Test.ml ) with other testsuites and
 * run via Alcotest.run.
 *)
val tests : (Fpath.t -> AST_generic.program) -> Testo.t list
