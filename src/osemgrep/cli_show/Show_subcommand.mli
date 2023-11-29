type caps = < stdout : Cap.Console.stdout ; network : Cap.Network.t >

(*
   Parse a semgrep-show command, execute it and exit.

   Usage: main [| "semgrep-show"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : caps -> string array -> Exit_code.t

(* called from main() but also from Scan_subcommand.ml to manage the legacy
 * way to show things (e.g., 'semgrep scan --show-supported-languages')
 *)
val run : caps -> Show_CLI.conf -> Exit_code.t
