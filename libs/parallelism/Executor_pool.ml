(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Modified from https://github.com/ocaml-multicore/eio *)
(* This version of the executor pool exists because the provided eio version
   does not handle uncaught exceptions gracefully. Specifically, if an uncaught
   exception occurs while running `f`, but not on the normal stack of `f`, then
   the worker cancels the entire switch. This is rare but can happen in cases
   where an exception is raised in effect handling code, e.g. eio scheduling
   code, where the callstack does not include the guard around the call to `f`.
   An example exception would be one raised in a gc alarm, memprof callback, or
   a signal handler.

   This modified version detects if there was an active job running when a
   domain exits, and if so, resolves it with the exception and respawns the
   domain to continue processing further jobs. *)
(*****************************************************************************)
(* Code *)
(*****************************************************************************)
module Log = Log_parallelism.Log
open Eio

type job =
  | Pack : {
      fn : unit -> 'a;
      w : ('a, exn) Result.t Promise.u;
      weight : int;
    }
      -> job

type t = { queue : job Stream.t }

let max_capacity = 1_000_000
let max_capacity_f = float max_capacity

(* This function is the core of executor_pool.ml.
   Each worker runs in its own domain,
   taking jobs from [queue] whenever it has spare capacity. *)
let run_worker { queue } active_job =
  Switch.run ~name:"run_worker" @@ fun sw ->
  let capacity = ref 0 in
  let condition = Condition.create () in
  (* The main worker loop. *)
  let rec loop () =
    while !capacity >= max_capacity do
      Condition.await_no_mutex condition
    done;
    let (Pack { fn; w; weight } as job) = Stream.take queue in
    (* Record the active job so if we receive an async exception we can
       restart the domain as mentioned in the prelude *)
    active_job := Some job;
    capacity := !capacity + weight;
    Option.iter (Promise.resolve_error w) (Switch.get_error sw);
    Fiber.fork ~sw (fun () ->
        ignore
          (Promise.try_resolve w
             (try Ok (fn ()) with
             | ex -> Error ex));
        (* Done with active job :) *)
        active_job := None;
        capacity := !capacity - weight;
        Condition.broadcast condition);
    (* Give a chance to other domains to start waiting on [queue]
       before we take another item *)
    Fiber.yield ();
    (loop [@tailcall]) ()
  in
  loop ()

let create ~sw ~domain_count domain_mgr =
  (* Match upstream Eio.Executor_pool's synchronous queue semantics using the
     public Stream API. A capacity of 0 blocks submitters until a worker is
     ready, avoiding idle polling and unbounded prefetching. *)
  let queue = Stream.create 0 in
  let t = { queue } in
  for _ = 1 to domain_count do
    (* Workers run as daemons to not hold the user's switch from completing.
       It's up to the user to hold the switch open (and thus, the executor pool)
       by blocking on the jobs issued to the pool. *)
    Fiber.fork_daemon ~sw (fun () ->
        let rec f () =
          let active_job = ref None in
          try
            Domain_manager.run domain_mgr (fun () -> run_worker t active_job)
          with
          | Cancel.Cancelled _ as exn ->
              let bt = Printexc.get_raw_backtrace () in
              Printexc.raise_with_backtrace exn bt
          | exn -> (
              (* If there was an active job then we didn't really mean to raise
                 an exception, so resolve the promise and restart.

                 It's SUPER important that if there WASN'T an active job we
                 handle things normally. When workers exit just because they are
                 done with their work, they raise Stdlib.Exit, so we have to be
                 careful not to catch that and try to restart again *)
              match !active_job with
              | Some (Pack { w; _ }) ->
                  Log.err (fun m ->
                      m
                        "Uncaught exception in worker during executor pool \
                         job: %s\n\
                         Resolving job promise with exception, and restarting \
                         worker to continue on remaining jobs..."
                        (Printexc.to_string exn));
                  ignore (Promise.try_resolve w (Error exn));
                  f ()
              | None ->
                  let bt = Printexc.get_raw_backtrace () in

                  Printexc.raise_with_backtrace exn bt)
        in
        f ())
  done;
  t

let enqueue { queue } ~weight fn =
  if not (weight >= 0. && weight <= 1.) (* Handles NaN *) then
    raise
      (Invalid_argument
         (Printf.sprintf "Executor_pool: weight %g not >= 0.0 && <= 1.0" weight))
  else
    let weight = Float.to_int (weight *. max_capacity_f) in
    let p, w = Promise.create () in
    Stream.add queue (Pack { fn; w; weight });
    p

let submit t ~weight fn = enqueue t ~weight fn |> Promise.await
let submit_exn t ~weight fn = enqueue t ~weight fn |> Promise.await_exn

let submit_fork ~sw t ~weight fn =
  (* [enqueue] blocks until the job is accepted, so we have to fork here. *)
  Fiber.fork_promise ~sw (fun () -> submit_exn t ~weight fn)
