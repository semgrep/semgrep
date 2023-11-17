(* See this modules dune comment for why this exists *)
val run : 'a Lwt.t -> 'a
(** [run promise] runs a LWT promise and returns its result. *)

val detach : ('a -> 'b) -> 'a -> 'b Lwt.t
(** [detach promise] runs a LWT promise in the background. *)

val init_preemptive : int -> int -> (string -> unit) -> unit
(** [init_preemptive min max log] initializes the LWT preemptive scheduler. *)

val set_engine : unit -> unit
(** [set_engine ()] sets the LWT engine to libev if on Unix.
    This is important as select/poll is not great, and can easily
    run out of FDs, crashing the LS.
  *)

val sleep : float -> unit Lwt.t
(** [sleep delay] returns a promise that will be resolved after [delay] seconds. *)
