(* We need Cap.time_limit because we timeout after 10s if the install fails
 * We need Cap.exec because we run semgrep -pro_version as part of
 * the install process.
 *)
type caps = < Cap.network ; Cap.time_limit ; Cap.exec >

(*
   Parse a semgrep-install-semgrep-pro command, execute it and exit.

   Usage: main [| "semgrep-semgrep-pro"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : < caps ; .. > -> string array -> Exit_code.t

(* internal *)
val run_conf : < caps ; .. > -> Install_semgrep_pro_CLI.conf -> Exit_code.t
