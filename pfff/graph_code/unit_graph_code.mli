
(* Returns the testsuite for this directory. To be concatenated by 
 * the caller (e.g. in pfff/main_test.ml ) with other testsuites and 
 * run via OUnit.run_test_tt().
 *)

(* The function passed as a parameter makes it possible to have
 * a limited form of circular dependency, see unit_matcher.mli comment.
 *)

val unittest: 
  graph_of_string:(string -> Graph_code.graph) ->
  OUnit.test
