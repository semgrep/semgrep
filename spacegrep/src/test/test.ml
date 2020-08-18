(*
   Entrypoint to run the unit tests from the command line.
*)

let test_suites : unit Alcotest.test list = [
  Parser.test;
  Matcher.test;
]

let main () =
  Spacegrep.Match.debug := true;
  Alcotest.run "spacegrep" test_suites

let () = main ()
