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
  host_cpus : int;
  available_cpus : int;
  recommended_multicore_domains : int;
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

let recommended_multicore_domains available_cpus =
  (* Max out number of cores used to 16 unless more are requested so as to
   not overload on large machines.
   TODO: is this still necessary now that we check for cgroup quotas?
   *)
  let domains = min 16 available_cpus in
  (* Each domain is made up of two pthreads, one for the application thread
   (the so-called "mutator") and another for the concurrent garbage
   collector. In early testing, the overhead for each GC thread seems to be
   about 0.10 - 0.15 CPUs per mutator thread.  This also does not take into
   account the overhead of cross-core traffic for cache coherence, atomics,
   etc.  (See SAF-2284 for tracking the work of understanding those issues.)

   This overhead can add up when we have lots of spare cores; we don't
   want to oversubscribe domains' backing threads by default.  Obviously,
   users can tune this depending on their exact setup but let's give them
   a plausible default value.
   *)
  domains |> Float.of_int |> Float.mul 0.85 |> Float.round |> Int.of_float

let get () =
  let host_cpus, available_cpus = detect_available_cpus () in
  let recommended_multicore_domains =
    recommended_multicore_domains available_cpus
  in
  { host_cpus; available_cpus; recommended_multicore_domains }
