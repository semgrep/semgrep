(*
   'semgrep install-semgrep-pro' command-line parsing.
*)

(*
   The result of parsing a 'semgrep install-semgrep-pro' command.
*)

type conf = { common : CLI_common.conf; custom_binary : string option }
[@@deriving show]

(*
   Usage: parse_argv [| "semgrep-install-semgrep-pro"; <args> |]

   Turn argv into a conf structure.
*)
val parse_argv : string array -> conf
