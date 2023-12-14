(*
   Command-line interface generated for a test program.

   For now at least, this is a simplified interface over what Alcotest
   supports.
*)

type subcommand_result =
  | Run_result of unit Types.test_with_status list
  | Status_result of unit Types.test_with_status list
  | Approve_result

(*
   Run a test suite and return an exit code.

   Usage:

     Cmd.interpret_argv tests |> exit
*)
val interpret_argv :
  ?argv:string array ->
  ?expectation_workspace_root:string ->
  ?handle_subcommand_result:(int -> subcommand_result -> unit) ->
  ?status_workspace_root:string ->
  project_name:string ->
  (unit -> unit Types.test list) ->
  unit
