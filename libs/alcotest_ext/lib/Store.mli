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

type output_file_pair = {
  short_name : string;
  path_to_expected_output : string;
  path_to_output : string;
}

(*
   For diffing output against expected output
*)
val get_output_file_pairs : 'a Types.test -> output_file_pair list

(*
   These functions are available after the call to 'init'.
*)
val get_status_workspace : unit -> string
val get_expectation_workspace : unit -> string
val get_status : 'a Types.test -> Types.status

(*
   Save a test result, typically from a test that was just run.

   Overwrites any previous result silently.
*)
val save_result : 'a Types.test -> Types.result -> unit

(*
   Remove the result from a possible previous run.
*)
val delete_result : 'a Types.test -> unit

(*
   Summarize the status of a test.

   The option 'accept_missing_expected_output' is for the special case of
   new tests for which we don't have a reference output yet. The default
   is false. Note however that a user typically wants the first output of
   the test to become the reference with no questions asked.
*)
val status_summary_of_status : Types.status -> Types.status_summary

(*
   Replace the expected output of a test with a satisfying outcome
   (pass or xfail).

   Returns an error message if the test status is not PASS or XFAIL.
*)
val approve_new_output : 'a Types.test -> (unit, string) Result.t

(**************************************************************************)
(* Wrappers for capturing test output *)
(**************************************************************************)

val with_result_capture : unit Types.test -> unit -> unit
val with_result_capture_lwt : unit Lwt.t Types.test -> unit -> unit Lwt.t
