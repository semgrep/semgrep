(*
   Test suite for testing alcotest_ext itself.
*)

(* We should consider a shorter name for this library. *)
let t = Alcotest_ext.create

let tests =
  [
    t "simple" (fun () -> ());
    t "capture stdout" ~output_kind:Stdout (fun () -> print_string "hello\n");
    t "capture stderr" ~output_kind:Stderr (fun () -> prerr_string "error\n");
    t "capture stdxxx" ~output_kind:Merged_stdout_stderr (fun () ->
        print_string "hello\n";
        flush stdout;
        prerr_string "error\n";
        flush stderr;
        print_string "goodbye\n");
    t "capture stdout and stderr" ~output_kind:Separate_stdout_stderr (fun () ->
        print_string "hello\n";
        prerr_string "error\n");
    t "xfail" ~expected_outcome:(Should_fail "raises exception on purpose")
      (fun () -> failwith "this exception is expected");
    t "skipped" ~skipped:true (fun () -> failwith "this shouldn't happen");
    t "chdir" ~tolerate_chdir:true (fun () -> Sys.chdir "/");
  ]

let () = Alcotest_ext.interpret_argv ~project_name:"alcotest_ext" tests |> exit
