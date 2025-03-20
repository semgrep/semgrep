(* Runs an RPC server that takes calls on stdin and sends results to stdout. *)
val main :
  < Cap.exec
  ; Cap.tmp
  ; Cap.network
  ; Cap.readdir
  ; Cap.random
  ; Core_scan.caps
  ; .. > ->
  unit
