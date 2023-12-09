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
val exit_semgrep : Cap.Process.exit -> reason -> 'a
