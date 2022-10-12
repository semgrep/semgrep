(*
   Parse a semgrep-scan command, execute it and exit.

   Translated from scan.py
   TODO and semgrep_main.py?
*)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (conf : Scan_CLI.conf) : Exit_code.t =
  (* !!!TODO!!! use the result!! see semgrep_main.py *)
  let _res = Core_runner.invoke_semgrep conf in
  Exit_code.ok

let main (argv : string array) : Exit_code.t =
  let res = Scan_CLI.parse_argv argv in
  (* LATER: this error handling could be factorized probably
   * between the different subcommands at some point
   *)
  match res with
  | Ok conf -> CLI_common.safe_run run conf
  | Error exit_code -> exit_code
