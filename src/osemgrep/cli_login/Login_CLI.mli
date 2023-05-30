(*
   'semgrep login' (and also 'semgrep logout') command-line parsing.
*)

(*
   The result of parsing a 'semgrep login/logout' command.
*)
type conf = {
  (* mix of --debug, --quiet, --verbose *)
  logging_level : Logs.level option;
}
[@@deriving show]

val login_cmdline_info : Cmdliner.Cmd.info
val logout_cmdline_info : Cmdliner.Cmd.info

(*
   Usage: parse_argv cmd_info [| "semgrep-login"; <args> |]

   Turn argv into a conf structure.

   This function may raise an exn in case of an error parsing argv
   but this should be caught by CLI.safe_run.
*)
val parse_argv : Cmdliner.Cmd.info -> string array -> conf
