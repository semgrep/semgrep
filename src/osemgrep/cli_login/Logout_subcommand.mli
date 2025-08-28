(*
   Copyright (c) 2022-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Parse a semgrep-logout command, execute it and exit.

   Usage: main caps [| "semgrep-logout"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : < Cap.stdout > -> string array -> Exit_code.t

(* internal *)
val run_conf : < Cap.stdout > -> Logout_CLI.conf -> Exit_code.t
