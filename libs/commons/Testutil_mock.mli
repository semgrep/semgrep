(* Utilities to use in tests to "mock" things (e.g., the environment, logs).

   See also Http_mock_client for mocking HTTP requests/responses.
*)

(* ex: [with_setenv "MY_ENV" "true" (fun () -> ...)] *)
val with_setenv : string -> string -> (unit -> 'a) -> 'a
