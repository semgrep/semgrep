(*
   'semgrep interactive' command-line parsing.
*)

(*
   The result of parsing a 'semgrep interactive' command.
*)

type conf = { logging_level : Logs.level option } [@@deriving show]

(*
   Usage: parse_argv [| "semgrep-interactive"; <args> |]

   Turn argv into a conf structure.

   This function may raise an exn in case of an error parsing argv
   but this should be caught by CLI.safe_run.
*)
val parse_argv : string array -> conf
