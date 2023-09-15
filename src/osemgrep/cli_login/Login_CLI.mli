(*
   'semgrep login' command-line parsing.
*)

(*
   The result of parsing a 'semgrep login' command.
*)
type conf = {
  common : CLI_common.conf;
      (* Initialize the auth exchange with a temporary shared secret *)
  init : string;
}
[@@deriving show]

val login_cmdline_info : Cmdliner.Cmd.info

(*
   Usage: parse_argv cmd_info [| "semgrep-login"; <args> |]

   Turn argv into a conf structure.

   This function may raise an exn in case of an error parsing argv
   but this should be caught by CLI.safe_run.
*)
val parse_argv : Cmdliner.Cmd.info -> string array -> conf
