(*
   Filter and run tests
*)

val to_alcotest : unit Alcotest_ext_types.test list -> unit Alcotest.test list

val to_alcotest_lwt :
  unit Lwt.t Alcotest_ext_types.test list -> unit Alcotest_lwt.test list

val run_tests :
  ?filter_by_substring:string -> unit Alcotest_ext_types.test list -> unit

val run_tests_lwt :
  ?filter_by_substring:string ->
  unit Lwt.t Alcotest_ext_types.test list ->
  unit Lwt.t

val list_status :
  ?filter_by_substring:string -> _ Alcotest_ext_types.test list -> unit

val approve_output :
  ?filter_by_substring:string -> _ Alcotest_ext_types.test list -> unit
