(* no exit, no argv
 * TODO: Cap.files_argv, Cap.domain, Cap.thread
 *)
type caps =
  < Cap.stdout
  ; Cap.network
  ; Cap.exec
  ; Cap.random
  ; Cap.signal
  ; Cap.tmp
  ; Cap.readdir
  ; Cap.chdir
  ; Cap.fork
  ; Cap.time_limit
  ; Cap.memory_limit >

(*
   Parse the semgrep command line, run the requested subcommand, and return
   an exit status.

   If called as a standalone program, the 'exit' function should be called
   with this exit status. If testing, the exit status can be checked
   against expectations.

   Exceptions are caught and turned into an appropriate exit code
   (unless you used --debug).
*)
val main : caps -> string array -> Exit_code.t

val hook_semgrep_publish :
  (< Cap.stdout ; Cap.network > -> string array -> Exit_code.t) Hook.t

val hook_semgrep_show : (caps -> string array -> Exit_code.t) Hook.t
