(*
   Parse a semgrep-install-semgrep-pro command, execute it and exit.

   Usage: main [| "semgrep-semgrep-pro"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : < Cap.network ; Cap.alarm > -> string array -> Exit_code.t

(* internal *)
val run_conf :
  < Cap.network ; Cap.alarm > -> Install_semgrep_pro_CLI.conf -> Exit_code.t
