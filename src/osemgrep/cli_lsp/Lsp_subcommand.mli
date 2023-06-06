(*
   Parse a semgrep-lsp command, execute it and exit.

   Usage: main [| "semgrep-lsp"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : string array -> Exit_code.t

(* internal *)
val run : Lsp_CLI.conf -> Exit_code.t
