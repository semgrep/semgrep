(*
   Probe the system to help determine an appropriate number of parallel jobs
   to run.
*)

type t = {
  host_cpus : int;
  available_cpus : int;
  recommended_parmap_jobs : int;
}
[@@deriving yojson]

let get_host_cpus () =
  (* OCaml 5 gives us the number of total system cores via this function.
     The documentation isn't clear but it's what it does and
     it's cross-platform. *)
  Domain.recommended_domain_count ()

(*
   Detect how many CPUs are available to the user assuming no other
   processes are running.

   This takes into account the quota imposed on the current cgroup on Linux.
   This is typically the case in Docker containers that share the host
   with other containers.
*)
let detect_available_cpus () =
  let host_cpus = get_host_cpus () in
  let available_cpus =
    (* check for cgroup quota *)
    match Cgroup_limits.get_max_cpus () with
    | Ok (CPU_limit num) ->
        (* round up to maximize CPU usage: 1.1 -> 2 *)
        let cgroup_limit = truncate (ceil num) in
        min cgroup_limit host_cpus
    | Ok No_CPU_limit
    | Error () ->
        host_cpus
  in
  (* assume at least one CPU is available otherwise we wouldn't even exist *)
  (host_cpus, max 1 available_cpus)

(*
   TODO: detect memory limit and reduce the number of concurrent jobs
   if too little memory is available?
*)
let recommend_number_of_parmap_jobs () =
  (*
     Hardcode num_jobs to 1 for non-unix (i.e. Windows) because
     we don't believe that Parmap works in those environments

     TODO: remove this limitation once we no longer use Parmap in favor of
     multicore OCaml. Don't forget to update the help text for -j.
  *)
  let host_cpus, available_cpus = detect_available_cpus () in
  let recommended_parmap_jobs =
    let num_usable_cpus = if Sys.unix then available_cpus else 1 in
    (*
       Max out number of cores used to 16 unless more are requested so as to
       not overload on large machines.
       TODO: is this still necessary now that we check for cgroup quotas?
     *)
    min 16 num_usable_cpus
  in
  { host_cpus; available_cpus; recommended_parmap_jobs }

let get () = recommend_number_of_parmap_jobs ()
