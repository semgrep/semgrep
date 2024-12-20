(*
   Parse a semgrep-scan command, execute it and exit.

   Usage: main caps [| "semgrep-scan"; ... |]

   This function returns an exit code to be passed to the 'exit' function.

   Note that this subcommand can also calls the 'test', 'validate', and 'show'
   subcommands when using legacy flags (e.g., with 'semgrep scan --test').
*)

type caps =
  < Cap.stdout
  ; Cap.network
  ; Cap.tmp
  ; Cap.chdir
  ; Cap.fork
  ; Cap.time_limit
  ; Cap.memory_limit >

val main : < caps ; .. > -> string array -> Exit_code.t

(* internal *)
val run_conf : < caps ; .. > -> Scan_CLI.conf -> Exit_code.t
val run_scan_conf : < caps ; .. > -> Scan_CLI.conf -> Exit_code.t

(* internal: also used in CI *)
val check_targets_with_rules :
  (* caps - network *)
  < Cap.stdout
  ; Cap.chdir
  ; Cap.tmp
  ; Cap.fork
  ; Cap.time_limit
  ; Cap.memory_limit
  ; .. > ->
  Scan_CLI.conf ->
  Profiler.t ->
  Rule_fetching.rules_and_origin list ->
  Fpath.t list
  * Semgrep_output_v1_t.core_error list
  * Semgrep_output_v1_t.skipped_target list ->
  ( Rule.rule list * Core_runner.result * Semgrep_output_v1_t.cli_output,
    Exit_code.t )
  result
