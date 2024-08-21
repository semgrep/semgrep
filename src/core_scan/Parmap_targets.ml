(* Yoann Padioleau
 *
 * Copyright (C) 2022-2024 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
module In = Input_to_core_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small Parmap wrapper with specialized handling for semgrep targets
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*
   Sort targets by decreasing size. This is meant for optimizing
   CPU usage when processing targets in parallel on a fixed number of cores.
*)

let sort_targets_by_decreasing_size (targets : Target.t list) : Target.t list =
  targets
  |> List_.sort_by_key
       (fun target -> UFile.filesize (Target.internal_path target))
       (* Flip the comparison so we get descending,
        * instead of ascending, order *)
       (Fun.flip Int.compare)

let sort_code_targets_by_decreasing_size (targets : Target.regular list) :
    Target.regular list =
  targets
  |> List_.sort_by_key
       (fun (target : Target.regular) ->
         UFile.filesize target.path.internal_path_to_content)
       (* Flip the comparison so we get descending,
        * instead of ascending, order *)
       (Fun.flip Int.compare)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* Run jobs in parallel, using number of cores specified with -j *)
let map_targets__run_in_forked_process_do_not_modify_globals (ncores : int)
    (f : Target.t -> 'a) (targets : Target.t list) : 'a list =
  (*
     Sorting the targets by decreasing size is based on the assumption
     that larger targets will take more time to process. Starting with
     the longer jobs allows parmap to feed the workers with shorter and
     shorter jobs, as a way of maximizing CPU usage.
     This is a kind of greedy algorithm, which is in general not optimal
     but hopefully good enough in practice.

     This is needed only when ncores > 1, but to reduce discrepancy between
     the two modes, we always sort the target queue in the same way.
  *)
  let targets = sort_targets_by_decreasing_size targets in
  if ncores <= 1 then List_.map f targets
  else (
    (*
       Parmap creates ncores children processes which listen for
       chunks of input. When a chunk size is specified, parmap feeds
       the ncores processes in small chunks of the specified size
       instead of just dividing the input list into exactly ncores chunks.

       Since our jobs are relatively big compared to the serialization
       and communication overhead, setting the chunk size to 1 works
       fine.  We don't want to have two giant target files in the same
       chunk, so this setting takes care of it.
    *)
    (* Quoting Parmap's README:
     * > To obtain maximum speed, Parmap tries to pin the worker processes to a CPU
     * Unfortunately, on the new Apple M1, and depending on the number of workers,
     * Parmap will enter an infinite loop trying (but failing) to pin a worker to
     * CPU 0. This only happens with HomeBrew installs, presumably because under
     * HomeBrew's build environment HAVE_MACH_THREAD_POLICY_H is set
     * (https://github.com/rdicosmo/parmap/blob/1.2.3/src/setcore_stubs.c#L47).
     * So, despite it may hurt perf a bit, we disable core pinning to work around
     * this issue until this is fixed in a future version of Parmap.
     *)
    Parmap.disable_core_pinning ();
    assert (ncores > 0);
    (* TODO: port this functionality to Logs:
       let init _ = Logging.add_PID_tag () in
    *)
    Logs.debug (fun m ->
        m "running parmap with %d cores on %d targets" ncores
          (List.length targets));
    Parmap.parmap ~ncores ~chunksize:1 f (Parmap.L targets))

(* TODO: remove duplication, maybe update Deep_scan to handle
 * Target.t, not just Target.regular like we do in Core_scan
 *)
let map_regular_targets__run_in_forked_process_do_not_modify_globals
    (ncores : int) (f : Target.regular -> 'a) (targets : Target.regular list) :
    'a list =
  let targets = sort_code_targets_by_decreasing_size targets in
  if ncores <= 1 then List_.map f targets
  else (
    Parmap.disable_core_pinning ();
    assert (ncores > 0);
    Logs.debug (fun m ->
        m "running parmap with %d cores on %d targets" ncores
          (List.length targets));
    Parmap.parmap ~ncores ~chunksize:1 f (Parmap.L targets))
