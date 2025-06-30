val tests : Legacy_session.caps -> Testo.t list
val lwt_tests : Legacy_session.caps -> Testo_lwt.t list

(* Shared with the main test suite.
   TODO: relocate to a more logical library? *)
val project_root : unit -> Fpath.t
