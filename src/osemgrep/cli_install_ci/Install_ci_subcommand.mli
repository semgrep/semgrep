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
(*
   Install semgrep in CI for a given repository.
*)

(* we need Cap.exec for calling 'git', 'gh', 'command' *)
type caps = < Cap.random ; Cap.chdir ; Cap.tmp ; Cap.exec >

(*
   Parse a semgrep-install-ci command, execute it and exit.

   Usage: main [| "semgrep-install-ci"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : < caps ; .. > -> string array -> Exit_code.t

(* internal *)
val run_conf : < caps ; .. > -> Install_ci_CLI.conf -> Exit_code.t
