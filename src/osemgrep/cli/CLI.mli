(*
   Parse the semgrep command line, run the requested subcommand, and return
   an exit status.

   If called as a standalone program, the 'exit' function should be called
   with this exit status. If testing, the exit status can be checked
   against expectations.

   Exceptions are caught and turned into an appropriate exit code
   (unless you used --debug).
*)
val main : Cap.all_caps -> string array -> Exit_code.t
