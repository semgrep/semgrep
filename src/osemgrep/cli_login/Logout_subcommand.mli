(*
   Parse a semgrep-logout command, execute it and exit.

   Usage: main [| "semgrep-logut"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : string array -> Exit_code.t

(* no parameters for now *)
type conf = unit

(* internal *)
val run : conf -> Exit_code.t
