(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Gather and print system resources available to semgrep.
*)

type t = {
  cpu : Num_jobs.t;
      (** Recommendations derived from CPU info and cgroup quotas *)
}

val resources : t
(** Resources record obtained by inspecting system resources at
    module initialization time. *)

val show : unit -> string
(** Text output showing resources (CPU, memory, ...) for the
    'semgrep show resources' subcommand. *)

val to_json : unit -> string
(** JSON output showing resources (CPU, memory, ...) for the
    'semgrep show resources --json' subcommand. *)
