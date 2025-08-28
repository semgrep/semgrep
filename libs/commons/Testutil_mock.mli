(*
   Copyright (c) 2023-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Utilities to use in tests to "mock" things (e.g., the environment, logs).

   See also Http_mock_client for mocking HTTP requests/responses.
*)

(* ex: [with_setenv "MY_ENV" "true" (fun () -> ...)] *)
val with_setenv : string -> string -> (unit -> 'a) -> 'a
