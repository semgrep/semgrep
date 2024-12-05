type caps = < Cap.stdout ; Cap.network ; Cap.tmp >

(*
   Parse a semgrep-show command, execute it and exit.

   Usage: main caps [| "semgrep-show"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
   we need the network for the 'semgrep show identity/deployment'
*)
val main : < caps ; .. > -> string array -> Exit_code.t

(* called from main() but also from Scan_subcommand.ml to manage the legacy
 * way to show things (e.g., 'semgrep scan --show-supported-languages')
 *)
val run_conf : < caps ; .. > -> Show_CLI.conf -> Exit_code.t
