(*
   'semgrep scan' subcommand

   The argument is an argv, i.e. an array of the form

     [| "semgrep-scan"; <args> |]

   where the first element (argv0) is ignored and args are the arguments
   that matter.

   This function returns an exit code to be passed to the 'exit' function.
   Exceptions are caught and turned into an appropriate exit code.
*)
val main : string array -> int
