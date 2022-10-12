(*
   Parse a semgrep-scan command, execute it and exit.
*)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (conf : Scan_CLI.conf) =
  let _res = Core_runner.invoke_semgrep conf in
  Exit_code.ok

let main argv = Scan_CLI.parse_and_run argv run
