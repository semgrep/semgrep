(*s: semgrep/finding/unit_files.mli *)
(*s: signature [[Unit_files.unittest]] *)
(* Returns the testsuite for this directory To be concatenated by 
 * the caller (e.g. in pfff/main_test.ml ) with other testsuites and 
 * run via OUnit.run_test_tt 
 *)
val unittest: 
  OUnit.test
(*e: signature [[Unit_files.unittest]] *)
(*e: semgrep/finding/unit_files.mli *)
