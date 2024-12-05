(* Cap.exec because we call the 'open' command line tool to open a URL *)
type caps = < Cap.stdout ; Cap.network ; Cap.exec >

(*
   Parse a semgrep-login command, execute it and exit.

   Usage: main caps [| "semgrep-login"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : < caps ; .. > -> string array -> Exit_code.t

(* internal *)
val run_conf : < caps ; .. > -> Login_CLI.conf -> Exit_code.t
