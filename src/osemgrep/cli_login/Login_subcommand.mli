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
(* Cap.exec because we call the 'open' command line tool to open a URL *)
type caps = < Cap.stdout ; Cap.network ; Cap.exec >

(*
   Parse a semgrep-login command, execute it and exit.

   Usage: main caps [| "semgrep-login"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : < caps ; .. > -> string array -> Exit_code.t

(* internal *)
val run_conf : < caps ; .. > -> Login_CLI.conf -> Exit_code.t
