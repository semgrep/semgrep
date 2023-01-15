(*
   Exit semgrep-core cleanly with appropriate logging and error messages.
*)

open Common

let logger = Logging.get_logger [ __MODULE__ ]

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
  logger#info "exit %i: %s" code msg;
  exit code
