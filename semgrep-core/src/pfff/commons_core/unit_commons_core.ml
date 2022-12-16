(*
   Test suite for this folder.
*)

let test_parallel_invoke_res () =
  Alcotest.(check int) "same int" 2 ((Parallel.invoke (fun x -> x + x) 1) ())

let test_parallel_invoke_exn () =
  try
    (Parallel.invoke (fun _ -> if true then raise Exit) ()) ();
    assert false
  with _unmatchable_exception ->
    ()

let test_parallel_invoke_crash () =
  try
    (Parallel.invoke
       (fun _ ->
          (*
             Killing self with sigsegv appears to not work in native code,
             but other signals cause process termination as expected.
             See discussion started at
             https://discuss.ocaml.org/t/delivering-sigsegv-to-self-in-native-code/7837
          *)
          Unix.kill (Unix.getpid ()) Sys.sigterm
       ) ()) ();
    Alcotest.fail "Parallel.invoke should have raised an exception."
  with
  | Failure errmsg ->
      (try
         Scanf.sscanf errmsg
           "process %i was killed by signal sigterm: Termination"
           (fun _pid -> ());
         ()
       with _ ->
         Alcotest.failf "Unexpected error message: %s\n%!" errmsg
      )
  | e ->
      Alcotest.failf "Not the exception we were expecting: %s\n%!"
        (Printexc.to_string e)

let tests =
  Testutil.pack_suites "commons_core" [
    Testutil.pack_tests "parallel" [
      "invoke res", test_parallel_invoke_res;
      "invoke exn", test_parallel_invoke_exn;
      "invoke crash", test_parallel_invoke_crash;
    ]
  ]
