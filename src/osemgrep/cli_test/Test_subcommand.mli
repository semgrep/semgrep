(*
   Parse a semgrep-test command, execute it and exit.

   Usage: main caps [| "semgrep-test"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : < Cap.stdout > -> string array -> Exit_code.t

(* called from main() but also from Scan_subcommand.ml to manage the legacy
 * way to show things (e.g., 'semgrep scan --tests <dir>')
 *)
val run_conf : < Cap.stdout > -> Test_CLI.conf -> Exit_code.t
