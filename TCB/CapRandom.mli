(* Capability-aware wrappers of the functions in Random.ml *)

val int : Cap.Misc.random -> int -> int
val get_state : Cap.Misc.random -> unit -> Random.State.t
val self_init : Cap.Misc.random -> unit -> unit
