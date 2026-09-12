(*
   Entrypoint to run the unit tests from the command line.
*)

let test_suites = List.flatten [
  Test_tree_sitter_gen.Test.test_suites;
  Test_tree_sitter_run.Test.test_suites;
]

let main () = Alcotest.run "ocaml-tree-sitter" test_suites

let () = main ()
