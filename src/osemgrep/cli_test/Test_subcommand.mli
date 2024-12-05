(* = Cap.stdout + Core_scan.caps | Deep_scan.caps *)
type caps =
  < Cap.stdout ; Cap.fork ; Cap.time_limit ; Cap.memory_limit ; Cap.tmp >

(*
   Parse a semgrep-test command, execute it and exit.

   Usage: main caps [| "semgrep-test"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : < caps ; .. > -> string array -> Exit_code.t

(* called from main() above but also from Scan_subcommand.ml to manage the
 * legacy way to test things (e.g., 'semgrep scan --tests <dir>')
 *)
val run_conf : < caps ; .. > -> Test_CLI.conf -> Exit_code.t

(* pro hooks *)
val hook_pro_init : (unit -> unit) ref
val hook_pro_scan : (Core_scan.caps -> Core_scan.func) ref

val hook_deep_scan :
  (< Cap.tmp ; Cap.fork ; Cap.time_limit ; Cap.memory_limit > ->
  Core_scan_config.t ->
  Fpath.t (* root *) ->
  Core_result.result_or_exn)
  ref
