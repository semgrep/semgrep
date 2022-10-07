(*
   Parse a semgrep-scan command, execute it and exit.
*)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (_conf : Scan_CLI.conf) =
  print_endline "hello";
  0

let main argv = Scan_CLI.parse_and_run argv run
