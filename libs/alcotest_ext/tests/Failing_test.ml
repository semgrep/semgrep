(*
   Dummy suite that fails so we can check that failing tests are reported
   nicely.
*)

let t = Alcotest_ext.create

let tests =
  [
    t "failing" (fun () -> failwith "oh no, I'm failing");
    t "failing to fail" ~expected_outcome:(Should_fail "<reasons>") (fun () ->
        ());
  ]

let () =
  Alcotest_ext.interpret_argv ~project_name:"alcotest_ext_dummy_failing_tests"
    (fun () -> tests)
