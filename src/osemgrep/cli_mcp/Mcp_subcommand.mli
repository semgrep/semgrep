type caps =
  < Core_scan.caps ; Cap.random ; Cap.network ; Cap.tmp ; Cap.readdir >

val hook_run_mcp : (caps -> Mcp_CLI.conf -> unit) option Hook.t

(*
   Parse a semgrep-mcp command, execute it and exit.

   Usage: main [| "semgrep-mcp"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : < caps ; .. > -> string array -> Exit_code.t

(* internal *)
val run_conf : < caps ; .. > -> Mcp_CLI.conf -> Exit_code.t
