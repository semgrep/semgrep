(*
   'semgrep login' command-line parsing.
*)

(*
   The result of parsing a 'semgrep login' command.
*)
type conf = {
  common : CLI_common.conf;
  (* Initialize the auth exchange with a temporary shared secret *)
  one_time_seed : string;
}
[@@deriving show]

(*
   Usage: parse_argv cmd_info [| "semgrep-login"; <args> |]

   Turn argv into a conf structure.

   This function may raise an exn in case of an error parsing argv
   but this should be caught by CLI.safe_run.
*)
val parse_argv : string array -> conf
