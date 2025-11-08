(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)

(**
   Measure how long this or that piece of code takes to run.

   Pad's poor's man profiler. See pfff's Main.ml for example of use
   and the -profile command-line flag
 *)

(** Configuration *)
type prof = ProfAll | ProfNone | ProfSome of string list

val profile : prof ref
(** Global configuration *)

type entry = {
  name : string;
  total_time : float;
      (** Total clock time in seconds. Divide by [float count]
          to get the mean. *)
  count : int;
}
(** Result

    Sorry, no detailed stats such as the median or other percentiles.
*)

val measure : string -> (unit -> 'a) -> 'a
(** Measure how long it takes to execute a block of code under the given
    name. *)

val export : unit -> entry list
(** Produce a list of all the timed entries, sorted by decreasing total
    time. *)

val report : unit -> string
(** Produce a human-readable report *)

val flags : unit -> (string * Arg.spec * string) list
(** To use with [Arg], to add a [-profile] that enables profiling *)

val log_diagnostics_and_gc_stats : unit -> unit
(** Log and print on stderr, usually called just before exit *)
