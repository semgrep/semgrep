(*
   Dummy suite that fails so we can check that failing tests are reported
   nicely.
*)

let t = Alcotest_ext.create

let tests =
  [
    t "failing" (fun () ->
        print_endline "<something being printed by the test>";
        failwith "oh no, I'm failing");
    t "failing to fail" ~expected_outcome:(Should_fail "<reasons>") (fun () ->
        print_string "<something being printed by the test>");
  ]

let () =
  Alcotest_ext.interpret_argv ~project_name:"alcotest_ext_dummy_failing_tests"
    (fun () -> tests)
