val tests :
  < Cap.random ; Cap.network ; Cap.tmp ; Cap.fork ; Cap.time_limit > ->
  Testo.t list

val lwt_tests :
  < Cap.random ; Cap.network ; Cap.tmp ; Cap.fork ; Cap.time_limit > ->
  Testo_lwt.t list

(* Shared with the main test suite.
   TODO: relocate to a more logical library? *)
val project_root : unit -> Fpath.t
