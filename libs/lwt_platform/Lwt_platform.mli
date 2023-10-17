(* See this modules dune comment for why this exists *)
val run : 'a Lwt.t -> 'a
(** [run promise] runs a LWT promise and returns its result. *)
