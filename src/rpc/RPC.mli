(* Runs an RPC server that takes calls on stdin and sends results to stdout. *)
(* - Cap.exec is needed to query Git for project contributions
   - Cap.network is needed to POST symbol analysis back to the App
*)
val main : < Cap.exec ; Cap.tmp ; Cap.network > -> unit
