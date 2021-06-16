(*
   Entrypoint to run the unit tests from the command line.
*)

let test_suites : unit Alcotest.test list =
  [ File_type.test; Parser.test; Matcher.test; Src_file.test ]

let main () =
  Spacegrep.Match.debug := true;
  Alcotest.run "spacegrep" test_suites

let () = main ()
