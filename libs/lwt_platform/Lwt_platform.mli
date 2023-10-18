(* See this modules dune comment for why this exists *)
val run : 'a Lwt.t -> 'a
(** [run promise] runs a LWT promise and returns its result. *)

val detach : ('a -> 'b) -> 'a -> 'b Lwt.t
(** [detach promise] runs a LWT promise in the background. *)

val init_preemptive : int -> int -> (string -> unit) -> unit
(** [init_preemptive min max log] initializes the LWT preemptive scheduler. *)
