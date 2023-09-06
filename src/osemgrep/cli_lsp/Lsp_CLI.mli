(*
   'semgrep lsp' command-line parsing.
*)

(*
   The result of parsing a 'semgrep lsp' command.
*)

type conf = { common : CLI_common.conf } [@@deriving show]

(*
   Usage: parse_argv [| "semgrep-lsp"; <args> |]

   Turn argv into a conf structure.

   This function may raise an exn in case of an error parsing argv
   but this should be caught by CLI.safe_run.
*)
val parse_argv : string array -> conf
