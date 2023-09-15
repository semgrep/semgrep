(*
   'semgrep logout' command-line parsing.
*)

(*
   The result of parsing a 'semgrep logout' command.
*)
type conf = { common : CLI_common.conf } [@@deriving show]

val logout_cmdline_info : Cmdliner.Cmd.info

(*
   Usage: parse_argv cmd_info [| "semgrep-logout"; <args> |]

   Turn argv into a conf structure.

   This function may raise an exn in case of an error parsing argv
   but this should be caught by CLI.safe_run.
*)
val parse_argv : Cmdliner.Cmd.info -> string array -> conf
