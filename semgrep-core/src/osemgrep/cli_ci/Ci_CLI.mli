(*
   'semgrep ci' command-line parsing.
*)

(*
   The result of parsing a 'semgrep ci' command.
*)

type conf = Scan_CLI.conf

(*
   Usage: parse_argv [| "semgrep-ci"; <args> |]

   This function returns an exit code to be passed to the 'exit' function
   if there was an error parsing argv (Exit_code.fatal) or when
   using semgrep ci --help (Exit_code.ok), and the conf otherwise if everything
   went fine.
*)
val parse_argv : string array -> (conf, Exit_code.t) result
