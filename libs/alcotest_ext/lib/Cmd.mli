(*
   Command-line interface generated for a test program.

   For now at least, this is a simplified interface over what Alcotest
   supports.
*)

(*
   Run a test suite and return an exit code.

   Usage:

     Cmd.interpret_argv tests |> exit
*)
val interpret_argv :
  ?argv:string array ->
  ?expectation_workspace_root:string ->
  ?status_workspace_root:string ->
  project_name:string ->
  unit Types.test list ->
  int
