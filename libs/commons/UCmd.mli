(* You should prefer to use the safer CapExec.ml module. This UCmd module
 * is for Unsafe use of Cmd (hence the name). see TCB/Cap.mli for more info.
 *)

(*
   The following functions capture the error output of the command being run
   and logs it as the info level, allowing it to be silenced by adjusting
   the log level.
*)
val string_of_run :
  trim:bool -> Cmd.t -> (string * Cmd.run_status, [> Rresult.R.msg ]) result

val lines_of_run :
  trim:bool ->
  Cmd.t ->
  (string list * Cmd.run_status, [> Rresult.R.msg ]) result

val status_of_run :
  ?quiet:bool -> Cmd.t -> (Bos.OS.Cmd.status, [> Rresult.R.msg ]) result

val with_open_process_in : string -> (in_channel -> 'a) -> 'a

(* old style *)
exception CmdError of Unix.process_status * string

val cmd_to_list : ?verbose:bool -> string -> string list

val string_of_run_with_stderr :
  trim:bool ->
  Cmd.t ->
  (string * Cmd.run_status, [> Rresult.R.msg ]) result * string
(** Like string_of_run but instead of logging the stderr output, it captures it and returns it (in both success and failure cases). *
 * The first part of the return type matches the return value of `string_of_run`; the last string part contains the stderr contents *)
