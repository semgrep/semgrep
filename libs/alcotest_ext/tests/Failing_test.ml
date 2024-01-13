(*
   Dummy suite that fails so we can check that failing tests are reported
   nicely.
*)

let t = Alcotest_ext.create

let failing_function () =
  print_endline "<something being printed by the test>";
  raise (Failure "oh no, I'm failing")

let tests =
  [
    t "failing" failing_function;
    t "failing to fail" ~expected_outcome:(Should_fail "<reasons>") (fun () ->
        print_string "<something being printed by the test>");
    t "output mismatch" ~checked_output:Stdout (fun () ->
        print_string
          {|
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat
non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
|});
    t "missing snapshot" ~checked_output:Stdout (fun () -> ());
  ]

let () =
  Alcotest_ext.interpret_argv ~project_name:"alcotest_ext_dummy_failing_tests"
    (fun () -> tests)
