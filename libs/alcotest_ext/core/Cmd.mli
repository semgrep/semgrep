(*
   Command-line interface generated for a test program.

   For now at least, this is a simplified interface over what Alcotest
   supports.
*)

type 'unit_promise subcommand_result =
  | Run_result of 'unit_promise Types.test_with_status list
  | Status_result of 'unit_promise Types.test_with_status list
  | Approve_result

(*
   Run a test suite and return an exit code.

   Usage:

     Cmd.interpret_argv tests |> exit
*)
val interpret_argv :
  ?argv:string array ->
  ?expectation_workspace_root:string ->
  ?handle_subcommand_result:(int -> 'unit_promise subcommand_result -> unit) ->
  ?status_workspace_root:string ->
  mona:'unit_promise Mona.t ->
  project_name:string ->
  (unit -> 'unit_promise Types.test list) ->
  'unit_promise
