(*
   Read-only interface to Linux-specific CPU and memory quotas

   Cgroups (control groups) are a Linux feature used notably by containers
   such Docker containers that restrict access to system resources.

   CPU and memory quotas associated with the current cgroup are available
   as special files. Two cgroup interfaces exist, v1 and v2.
   This module detects which cgroup version is being used if any, and
   reads the quotas if any.
*)

type cgroup_files = {
  cpu_quota_path_v1 : Fpath.t option;
  cpu_period_path_v1 : Fpath.t option;
  cpu_quota_period_path_v2 : Fpath.t option;
}
(** Paths to the specials files exposed by Linux that we read from to
    extract the quotas for the current cgroup.

    This is provided for emulating cgroups v1 or v2 in tests.
*)

type max_cpus =
  | No_CPU_limit
  | CPU_limit of float  (** a positive and finite float *)

val get_max_cpus : ?cgroup_files:cgroup_files -> unit -> (max_cpus, unit) result
(** Return the maximum number of CPUs the current cgroup could use.
    An error is returned if we're not in a cgroup or on a system that
    doesn't support cgroups.
    The result is only a limit, not a guarantee that the host has that
    many CPUs. To get the system-wide CPU count, consider using
    [Domain.recommended_domain_count].

    The option [cgroup_files] is for testing only.
*)

(* TODO: get memory quota? *)
