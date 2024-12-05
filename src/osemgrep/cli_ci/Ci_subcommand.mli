type caps =
  < Cap.stdout
  ; Cap.network
  ; Cap.exec
  ; Cap.tmp
  ; Cap.chdir
  ; Cap.fork
  ; Cap.time_limit
  ; Cap.memory_limit >

(*
   Parse a semgrep-ci command, execute it and exit.

   Usage: main [| "semgrep-ci"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : < caps ; .. > -> string array -> Exit_code.t

(* internal *)
val run_conf : < caps ; .. > -> Ci_CLI.conf -> Exit_code.t

(* used by Test_is_blocking_Helpers.ml used itself in Test.ml *)
val rule_is_blocking : JSON.t -> bool
val finding_is_blocking : Semgrep_output_v1_t.cli_match -> bool

(* used by Unit_ci.ml *)
val scan_metadata : unit -> Semgrep_output_v1_t.scan_metadata
