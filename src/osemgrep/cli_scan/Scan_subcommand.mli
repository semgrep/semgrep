(*
   Parse a semgrep-scan command, execute it and exit.

   Usage: main [| "semgrep-scan"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : string array -> Exit_code.t

(* internal *)
val run : Scan_CLI.conf -> Exit_code.t

(* internal: scan all the files - also used in CI *)
val scan_files :
  Rule_fetching.rules_and_origin list ->
  Scan_CLI.conf ->
  ( Rule.rule list * Core_runner.result * Semgrep_output_v1_t.cli_output,
    Exit_code.t )
  result
