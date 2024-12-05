(* Cap.stdout + Core_scan.caps + network (we run metachecking rules) *)
type caps =
  < Cap.stdout
  ; Cap.network
  ; Cap.tmp
  ; Cap.fork
  ; Cap.time_limit
  ; Cap.memory_limit >

(*
   Parse a semgrep-validate command, execute it and exit.

   Usage: main caps [| "semgrep-validate"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : < caps ; .. > -> string array -> Exit_code.t

(* called from main() above but also from Scan_subcommand.ml to manage the
 * legacy way to test things (e.g., 'semgrep scan --validate <dir>')
 *)
val run_conf : < caps ; .. > -> Validate_CLI.conf -> Exit_code.t

(* pro hooks *)
val hook_pro_init : (unit -> unit) ref
