(*
   Manage the storage of test statuses and test results.
   Much of this is private to this library.
*)

(*
   This function must be called exactly once to define where things are
   stored. It doesn't write to the file system.
*)
val init_settings :
  ?expectation_workspace_root:string ->
  ?status_workspace_root:string ->
  project_name:string ->
  unit ->
  unit

(*
   Create missing folders.
*)
val init_workspace : unit -> unit

(*
   These functions are available after the call to 'init'.
*)
val get_status_workspace : unit -> string
val get_expectation_workspace : unit -> string
val get_status : 'a Alcotest_ext_types.test -> Alcotest_ext_types.status

(*
   Save a test result, typically from a test that was just run.

   Overwrites any previous result silently.
*)
val save_result :
  'a Alcotest_ext_types.test -> Alcotest_ext_types.result -> unit

(*
   Remove the result from a possible previous run.
*)
val delete_result : 'a Alcotest_ext_types.test -> unit

(*
   Summarize the status of a test.

   The option 'accept_missing_expected_output' is for the special case of
   new tests for which we don't have a reference output yet. The default
   is false. Note however that a user typically wants the first output of
   the test to become the reference with no questions asked.
*)
val status_class_of_status :
  ?accept_missing_expected_output:bool ->
  Alcotest_ext_types.status ->
  Alcotest_ext_types.status_class

(*
   Replace the expected output of a test with a satisfying outcome
   (pass or xfail).

   Returns an error message if the test status is not PASS or XFAIL.
*)
val approve_new_output : 'a Alcotest_ext_types.test -> (unit, string) Result.t

(**************************************************************************)
(* Wrappers for capturing test output *)
(**************************************************************************)

val with_result_capture : unit Alcotest_ext_types.test -> unit -> unit

val with_result_capture_lwt :
  unit Lwt.t Alcotest_ext_types.test -> unit -> unit Lwt.t
