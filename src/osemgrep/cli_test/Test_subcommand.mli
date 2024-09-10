(* = Cap.stdout + Core_scan.caps *)
type caps = < Cap.stdout ; Cap.fork ; Cap.alarm >

(*
   Parse a semgrep-test command, execute it and exit.

   Usage: main caps [| "semgrep-test"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : caps -> string array -> Exit_code.t

(* called from main() above but also from Scan_subcommand.ml to manage the
 * legacy way to test things (e.g., 'semgrep scan --tests <dir>')
 *)
val run_conf : caps -> Test_CLI.conf -> Exit_code.t
