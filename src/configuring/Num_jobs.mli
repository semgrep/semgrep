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
