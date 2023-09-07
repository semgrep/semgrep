(*
   Install semgrep in CI for a given repository.
*)

(*
   Parse a semgrep-install-ci command, execute it and exit.

   Usage: main [| "semgrep-install-ci"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : string array -> Exit_code.t

(* internal *)
val run : Install_CLI.conf -> Exit_code.t
