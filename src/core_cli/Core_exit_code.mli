(*
   Copyright (c) 2021-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Exit semgrep-core cleanly with appropriate logging and error messages.
*)

(*
   Exit statuses used when exiting semgrep explicitly.
*)
type reason =
  | Success (* 0 *)
  | False (* non-zero: not really an error, just a test returning false. *)
  | Bad_command_line
  | Unknown_exception of Exception.t

(*
   Call the 'exit' function with the appropriate exit code and logging.
*)
val exit_semgrep : reason -> 'a
