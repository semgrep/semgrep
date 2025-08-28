(*
   Copyright (c) 2023-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
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
