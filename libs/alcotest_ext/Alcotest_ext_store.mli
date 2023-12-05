(*
   Manage the storage of test statuses and test results.
   Much of this is private to this library.
*)

(*
   This function must be called exactly once to define where things are
   stored.
*)
val init :
  ?status_workspace:string -> ?expectation_workspace:string -> unit -> unit

(*
   These functions are available after the call to 'init'.
*)
val get_status_workspace : unit -> string
val get_expectation_workspace : unit -> string
