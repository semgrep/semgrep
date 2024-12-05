(*
   The following functions capture the error output of the command being run
   and logs it as the info level, allowing it to be silenced by adjusting
   the log level.
*)
val string_of_run :
  Cap.Exec.t ->
  trim:bool ->
  Cmd.t ->
  (string * Cmd.run_status, [> Rresult.R.msg ]) result

val string_of_run_with_stderr :
  Cap.Exec.t ->
  trim:bool ->
  Cmd.t ->
  (string * Cmd.run_status, [> Rresult.R.msg ]) result * string
(** Like string_of_run but instead of logging the stderr output, it captures it and returns it (in both success and failure cases). *
 * The first part of the return type matches the return value of `string_of_run`; the last string part contains the stderr contents *)

val lines_of_run :
  Cap.Exec.t ->
  trim:bool ->
  Cmd.t ->
  (string list * Cmd.run_status, [> Rresult.R.msg ]) result

val status_of_run :
  Cap.Exec.t ->
  ?quiet:bool ->
  Cmd.t ->
  (Bos.OS.Cmd.status, [> Rresult.R.msg ]) result

(* Deprecated: you should use lines_of_run *)
val cmd_to_list : Cap.Exec.t -> ?verbose:bool -> string (* cmd *) -> string list

(* Deprecated: you should probably use one of the xxx_of_run above *)
val with_open_process_in :
  Cap.Exec.t -> string (* cmd *) -> (in_channel -> 'a) -> 'a
