(* You should prefer to use the safer CapExec.ml module. This UCmd module
 * is for Unsafe use of Cmd (hence the name). see TCB/Cap.mli for more info.
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
