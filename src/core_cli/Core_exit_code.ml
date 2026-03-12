(*
   Copyright (c) 2021-2025 Semgrep Inc.

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

open Common

(* Add new cases as needed *)
type reason =
  | Success
  | False
  | Bad_command_line
  | Unknown_exception of Exception.t

let string_of_reason = function
  | Success -> "success"
  | False -> "false"
  | Bad_command_line -> "bad command line"
  | Unknown_exception e -> spf "unknown exception %s" (Exception.to_string e)

(*
   TODO: standardize these error codes to help the semgrep wrapper explain
   what went wrong? Normally, errors are encoded in the json response but
   it's not always possible.
*)
let code_of_reason = function
  | Success -> 0
  | False -> 1
  | Bad_command_line -> 1
  | Unknown_exception _ -> (* same code as an uncaught exception *) 2

let exit_semgrep reason =
  let code = code_of_reason reason in
  let msg = string_of_reason reason in
  Logs.info (fun m -> m "exit %i: %s" code msg);
  raise (UnixExit (code, msg))
