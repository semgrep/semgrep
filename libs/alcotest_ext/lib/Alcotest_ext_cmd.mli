(*
   Command-line interface generated for a test program.

   For now at least, this is a simplified interface over what Alcotest
   supports.
*)

(*
   Run a test suite and return an exit code.

   Usage:

     Alcotest_ext_cmd.interpret_argv tests |> exit
*)
val interpret_argv :
  ?argv:string array ->
  ?expectation_workspace:string ->
  ?name:string ->
  ?status_workspace:string ->
  unit Alcotest_ext_types.test list ->
  int
