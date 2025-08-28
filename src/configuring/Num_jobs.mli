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
   Probe the system to help determine an appropriate number of parallel jobs
   to run.
*)

type t = {
  host_cpus : int;  (** informational. *)
  available_cpus : int;
      (** Number of CPUs available to the process, taking into account
        cgroup quotas on Linux.
        This is the smallest value that maximizes CPU usage in the
        absence of any other active process. *)
  recommended_parmap_jobs : int;
      (** The recommended value for running Parmap.
        This is set to 1 on Windows. *)
}
[@@deriving yojson]

val get : unit -> t
(**
   Check system resources and recommend number of parallel jobs for this
   or that purpose.
*)
