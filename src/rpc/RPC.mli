(* Runs an RPC server that takes calls on stdin and sends results to stdout. *)
val main :
  < Cap.exec
  ; Cap.tmp
  ; Cap.network
  ; Cap.fork
  ; Cap.readdir
  ; Cap.memory_limit
  ; Cap.time_limit > ->
  unit
