(*
   Dummy suite exercising a variety of test options.
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
    t ~output_kind:Stdout ~mask_output:String.lowercase_ascii "masked"
      (fun () -> print_endline "HELLO");
  ]

let () =
  Alcotest_ext.interpret_argv ~project_name:"alcotest_ext_dummy_tests"
    ~handle_subcommand_result:(fun exit_code _ ->
      print_endline "<handling result before exiting>";
      (* nosemgrep: forbid-exit *)
      exit exit_code)
    (fun () -> tests)
