(*
   Entrypoint to run the unit tests from the command line.
*)

let test_suites : unit Alcotest.test list = [
  Parser.test;
]

let main () = Alcotest.run "spacegrep" test_suites

let () = main ()
