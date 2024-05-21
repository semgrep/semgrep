val tests : < Cap.random ; Cap.network ; Cap.tmp > -> Testo.t list
val lwt_tests : < Cap.random ; Cap.network ; Cap.tmp > -> Testo_lwt.t list

(* Shared with the main test suite.
   TODO: relocate to a more logical library? *)
val get_project_root : unit -> Fpath.t
