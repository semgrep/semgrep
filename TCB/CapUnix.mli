(* Capability-aware wrappers of the dangerous functions in Unix.ml *)

(* See also libs/commons/CapExec.ml *)
val execvp : Cap.Exec.t -> string -> string array -> 'a

(* You should use CapExec.ml instead *)
val system : Cap.Exec.t -> string -> Unix.process_status
val fork : Cap.Process.fork -> unit -> int
val alarm : Cap.Process.time_limit -> int -> int

val setitimer :
  Cap.Process.time_limit ->
  Unix.interval_timer ->
  Unix.interval_timer_status ->
  Unix.interval_timer_status
