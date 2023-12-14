(*
   Filter and run tests
*)

type status_output_style = Short | Full

val to_alcotest : unit Types.test list -> unit Alcotest.test list
val to_alcotest_lwt : unit Lwt.t Types.test list -> unit Alcotest_lwt.test list

val run_tests :
  ?filter_by_substring:string ->
  ?lazy_:bool ->
  unit Types.test list ->
  int * unit Types.test_with_status list

val run_tests_lwt :
  ?filter_by_substring:string ->
  ?lazy_:bool ->
  unit Lwt.t Types.test list ->
  (int * unit Lwt.t Types.test_with_status list) Lwt.t

(* Print the status of each test.
   Return a non-zero exit status if any of the tests is not a success
   (PASS or XFAIL). *)
val list_status :
  ?filter_by_substring:string ->
  ?output_style:status_output_style ->
  'a Types.test list ->
  int * 'a Types.test_with_status list

val approve_output : ?filter_by_substring:string -> _ Types.test list -> int
