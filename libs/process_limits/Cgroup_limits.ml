(*
   Minimal access to Linux files giving us CPU and memory quotas if any.
*)

open Common

(*
   v2 example:

     # cat /sys/fs/cgroup/cpu.max
     150000 100000
     ^^^^^^ ^^^^^^
     QUOTA  PERIOD

   Another v2 example:

     # cat /sys/fs/cgroup/cpu.max
     max 100000
     ^^^
     no limit

   A CPU usage limit is specified by two numbers:
   - QUOTA (in microseconds): maximum CPU time that can be used during PERIOD
   - PERIOD (in microseconds): clock time

   QUOTA/PERIOD is a float representing the maximum number of CPUs.

   Format:
   v1: one file for the quota, one file for the period
   v2: one line of the form "QUOTA PERIOD"

   Quota and period are either decimal integer literals ([0-9]+) or the string
   "max". The unit is microseconds.
*)

type max_cpus = No_CPU_limit | CPU_limit of float

(* Paths are configurable so we can test our implementation without running
   booting a new Linux kernel and/or Docker container for each situation. *)
type cgroup_files = {
  cpu_quota_path_v1 : Fpath.t option;
  cpu_period_path_v1 : Fpath.t option;
  cpu_quota_period_path_v2 : Fpath.t option;
}

(* The real paths provided by cgroups v1 and v2 *)
let default_cgroup_files =
  {
    cpu_quota_path_v1 = Some (Fpath.v "/sys/fs/cgroup/cpu/cpu.cfs_quota_us");
    cpu_period_path_v1 = Some (Fpath.v "/sys/fs/cgroup/cpu/cpu.cfs_period_us");
    cpu_quota_period_path_v2 = Some (Fpath.v "/sys/fs/cgroup/cpu.max");
  }

let read_file opt_path =
  match opt_path with
  | None -> Error ()
  | Some path -> (
      try Ok (UFile.read_file path) with
      | _ -> Error ())

let is_positive_number x = x > 0. && Float.is_finite x

(* v1: need to allow -1 and positive values *)
let number_of_string str =
  match Float.of_string_opt (String.trim str) with
  | Some x when Float.is_finite x -> Ok x
  | _ -> Error ()

(* need to ignore the trailing newline if any (in v1, doesn't matter for v2) *)
let positive_number_of_string str =
  match Float.of_string_opt (String.trim str) with
  | Some x when is_positive_number x -> Ok x
  | _ -> Error ()

(* v1: if quota is -1, there's no limit *)
let divide_limits_v1 quota period =
  if quota < 0. then Ok No_CPU_limit
  else
    let r = quota /. period in
    if is_positive_number r then Ok (CPU_limit r) else Error ()

let get_max_cpus_v1 cgroup_files =
  match
    ( read_file cgroup_files.cpu_quota_path_v1,
      read_file cgroup_files.cpu_period_path_v1 )
  with
  | Ok a, Ok b ->
      let/ a = number_of_string a in
      let/ b = positive_number_of_string b in
      divide_limits_v1 a b
  | _ -> Error ()

let divide_limits_v2 quota period =
  let r = quota /. period in
  if is_positive_number r then Ok (CPU_limit r) else Error ()

let get_max_cpus_v2 cgroup_files =
  match read_file cgroup_files.cpu_quota_period_path_v2 with
  | Ok str -> (
      match String.split_on_char ' ' str with
      | [ "max"; _ ] -> Ok No_CPU_limit
      | [ a; b ] ->
          let/ a = positive_number_of_string a in
          let/ b = positive_number_of_string b in
          divide_limits_v2 a b
      | _ -> Error ())
  | Error () -> Error ()

(* Try v2, then v1. If both fail, assume it's because we're not in a cgroup
   or we're not even on Linux. *)
let get_max_cpus ?(cgroup_files = default_cgroup_files) () =
  match get_max_cpus_v2 cgroup_files with
  | Ok _ as res -> res
  | Error () -> get_max_cpus_v1 cgroup_files
