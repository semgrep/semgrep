(*
   'semgrep scan' command-line parsing.
*)

(*
   The result of parsing a 'semgrep scan' command.
*)
type conf = {
  autofix : bool;
  baseline_commit : string option;
  config : string option;
  lang : string option;
  metrics : Metrics.State.t;
  pattern : string option;
}

(*
   Usage: parse_and_run [| "semgrep-scan"; <args> |] run

   This function returns an exit code to be passed to the 'exit' function.
   Exceptions are caught and turned into an appropriate exit code.
*)
val parse_and_run : string array -> (conf -> int) -> int
