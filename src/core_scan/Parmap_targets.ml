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
 * See: Parmap_.mli for why the entrypoint functions returns a result list!
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

(* Helper to make all the results from a simple n=1 job similar to parmap's
   result *)
let wrap_with_ok (f : 'b -> 'a) (x : 'b) : ('a, 'c) result = Ok (f x)

let core_error_of_path_exc internal_path e =
  let exn = Exception.get_exn e in
  Logs.err (fun m ->
      m "exception on %s (%s)"
        (Fpath.to_string internal_path)
        (Printexc.to_string exn));
  Core_error.exn_to_error None internal_path e

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* Why do we need init and finalize here? TL;DR; we will segfault randomly when
   tracing is enabled an Parmap is run.

   Parmap works by forking, and then running the function passed to it in the
   child processes, with the xs passed by the parent. Tracing works by calling
   out to c code (curl) that sends data to some telemetry endpoint. This C code
   is called on every GC compaction. For some reason, when we fork, some data is
   not marked as freed, and randomly when we hit a GC cycle the curl code will
   try to free it and segfault.

   So what we must do is, before forking, shut down Tracing to remove the curl
   code that runs on a GC cycle, then after all forking is done we can re-enable
   it in both the parent and child processes. This basically resets the curl
   code and all associated memory so we won't segfault.

   Tracing shuts down and then restarts normally in the parent process once it
   exits as the setup is protected by a Common.protect. In the children though,
   this is not possible due to the architecture of parmap, so we must manually
   setup and teardown tracing via the init and finalize functions, which are
   called by parmap in the child process once before and after f(xs) is
   processed respectively.

   TODO: once we have ocaml 5 with domains we can hopefully rip all this out
   :)
*)

let parmap_child_top_level_span = ref None

let init job =
  (* Restart tracing as it is paused before forking below in both
     map_targets___* funcs *)
  (* NOTE: this only restarts tracing in the child *)
  Tracing.restart_tracing ();
  (* Keep track of how long the child runs *)
  parmap_child_top_level_span :=
    Some
      (Tracing.enter_span ~__FILE__ ~__LINE__
         (Printf.sprintf "parmap_job_%d" job))

let finalize () =
  !parmap_child_top_level_span |> Option.iter Tracing.exit_span;
  (* Stop tracing to ensure traces are flushed *)
  (* NOTE: this only stops tracing in the child *)
  Tracing.stop_tracing ()

(* Run jobs in parallel, using number of cores specified with -j *)
let map_targets__run_in_forked_process_do_not_modify_globals caps (ncores : int)
    (f : Target.t -> 'a) (targets : Target.t list) :
    ('a, Target.t * Core_error.t) result list =
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
  if ncores <= 1 then List_.map (wrap_with_ok f) targets
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
    Parmap_.disable_core_pinning ();
    assert (ncores > 0);
    (* TODO: port this functionality to Logs:
       let init _ = Logging.add_PID_tag () in
    *)
    Logs.debug (fun m ->
        m "running parmap with %d cores on %d targets" ncores
          (List.length targets));
    (* Default to core_error and the target here since that's what's most
       usefule in Core_scan. Maybe we should instead pass this as a
       parameter? *)
    let exception_handler (x : Target.t) (e : Exception.t) :
        Target.t * Core_error.t =
      let internal_path = Target.internal_path x in
      (x, core_error_of_path_exc internal_path e)
    in
    (* We must pause tracing here as forking with tracing on causes segfaults.
       See comments on this function in Tracing.ml *)
    Tracing.with_tracing_paused (fun () ->
        Parmap_.parmap caps ~init ~finalize ~ncores ~chunksize:1
          ~exception_handler f targets))

(* remove duplication? But we now use this function below at a few places like
 * in Deep_scan_phases and it would be uglier to wrap with [Target.Regular]
 * and then unwrap in [f].
 *)
let map_regular_targets__run_in_forked_process_do_not_modify_globals caps
    (ncores : int) (f : Target.regular -> 'a) (targets : Target.regular list) :
    ('a, Core_error.t) result list =
  let targets = sort_code_targets_by_decreasing_size targets in
  if ncores <= 1 then List_.map (wrap_with_ok f) targets
  else (
    Parmap_.disable_core_pinning ();
    assert (ncores > 0);
    Logs.debug (fun m ->
        m "running parmap with %d cores on %d targets" ncores
          (List.length targets));
    (* Default to core_error here. Maybe we should instead pass this as a
       parameter? *)
    let exception_handler
        ({ path = { internal_path_to_content; _ }; _ } : Target.regular)
        (e : Exception.t) : Core_error.t =
      core_error_of_path_exc internal_path_to_content e
    in
    (* We must pause tracing here as forking with tracing on causes segfaults.
       See comments on this function in Tracing.ml *)
    Tracing.with_tracing_paused @@ fun () ->
    Parmap_.parmap caps ~init ~finalize ~ncores ~chunksize:1 ~exception_handler
      f targets)
