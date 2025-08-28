(*
   Copyright (c) 2022-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* no exit, no argv
 * TODO: Cap.files_argv, Cap.domain, Cap.thread
 *)
type caps =
  < Cap.stdout
  ; Cap.network
  ; Cap.exec
  ; Cap.random
  ; Cap.signal
  ; Cap.tmp
  ; Cap.readdir
  ; Cap.chdir
  ; Cap.fork
  ; Cap.time_limit
  ; Cap.memory_limit >

(*
   Parse the semgrep command line, run the requested subcommand, and return
   an exit status.

   If called as a standalone program, the 'exit' function should be called
   with this exit status. If testing, the exit status can be checked
   against expectations.

   Exceptions are caught and turned into an appropriate exit code
   (unless you used --debug).
*)
val main : caps -> string array -> Exit_code.t

val hook_semgrep_publish :
  (< Cap.stdout ; Cap.network > -> string array -> Exit_code.t) Hook.t

val hook_semgrep_show : (caps -> string array -> Exit_code.t) Hook.t
