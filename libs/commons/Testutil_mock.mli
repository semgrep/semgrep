(* Utilities to use in tests to "mock" things (e.g., the environment, logs).

   See also Http_mock_client for mocking HTTP requests/responses.
*)

(* ex: [with_setenv "MY_ENV" "true" (fun () -> ...)] *)
val with_setenv : string -> string -> (unit -> 'a) -> 'a

(* [with_mocked_logs ~f ~final] will execute [f] in an environment
 * where [Logs_helpers.setup_logging()] is mostly converted in a noop and
 * where logs are stored in a buffer. The content of this buffer is
 * then accessible to the [final] function after [f] has finished
 * and can be inspected to assert certain log events occured.
 *)
val with_mocked_logs : f:(unit -> 'a) -> final:(string -> 'a -> unit) -> unit
