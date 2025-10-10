(* Copyright (C) 2025 Semgrep Inc.
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

(** Similar to Commons.memoized, but also consumes a mutex in order to ensure
 * safe current access to the memoized state.
 *
 * SAFETY: the function to be memoized must be threadsafe (in addition to being
 * deterministic); and, the computed and cached value must be safe to access
 * across threads.
 *)

(*****************************************************************************)
(* Metrics *)
(*****************************************************************************)

module SharedMemo_meter = (val Ometrics.make_meter Ometrics.default_meter_meta)

module Elements =
  (val SharedMemo_meter.make_int_counter
         (Ometrics.make_instrument_meta
            ~name:"semgrep.scan.parallelism.sharedmemo.elements"
            ~description:"How many elements are backed by the SharedMemo cache?"
            ()))

module HitRate =
  (val SharedMemo_meter.make_float_counter
         (Ometrics.make_instrument_meta
            ~name:"semgrep.scan.parallelism.sharedmemo.hitrate"
            ~description:
              "What proportion of SharedMemo accesses were previously memoized?"
            ()))

type metrics_state = {
  attrs : (string * Telemetry.user_data) list;
  accesses : int Atomic.t;
  misses : int Atomic.t;
}

let make_state id =
  {
    attrs = [ ("shared_memo_id", `String id) ];
    accesses = Atomic.make 0;
    misses = Atomic.make 0;
  }

let hit_rate accesses misses =
  let misses = Float.of_int (Atomic.get misses) in
  let accesses = Float.of_int (Atomic.get accesses) in
  let miss_rate = misses /. accesses in
  1.0 -. miss_rate

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let call_and_remember { attrs; accesses; misses } mtx ht key_fn f k =
  let k' = key_fn k in
  Atomic.incr accesses;
  (* Assumption: [k] will more often than not be a cache hit.  As a result,
   * in the unlikely event of a cache miss, we pay the cost of unlocking and
   * relocking to insert the new kv pair. *)
  let v =
    match Mutex.protect mtx (fun () -> Hashtbl.find_opt ht k') with
    | Some v -> v
    | None ->
        (* Note: f could well be an expensive computation, so do not starve
         * other accesses to the hashtable while we call it by holding the mutex.
         *
         * This does leave open the possibility that two threads will race on
         * computation on the same key: whoever gets there first will not be
         * overridden by the straggler.  Since f has to be deterministic, this
         * is fine (and while unfortunate, still preferable to holding the lock
         * through the computation. *)
        let v = f k in
        Atomic.incr misses;
        Mutex.protect mtx (fun () ->
            match Hashtbl.find_opt ht k' with
            (* Someone beat us to the insert! So it goes; discard our copy. *)
            | Some v' -> v'
            | None ->
                Hashtbl.add ht k' v;
                Elements.record ~attrs (Hashtbl.length ht);
                v)
  in
  HitRate.record ~attrs (hit_rate accesses misses);
  v

let make_with_state ~metrics_id mtx ht =
  let key_fn = Fun.id in
  let metrics = make_state metrics_id in
  call_and_remember metrics mtx ht key_fn

let make_with_key_fn ~metrics_id key_fn =
  let mtx = Mutex.create () in
  let ht = Hashtbl.create 101 in
  let metrics = make_state metrics_id in
  call_and_remember metrics mtx ht key_fn

let make ~metrics_id f x =
  let mtx = Mutex.create () in
  let ht = Hashtbl.create 101 in
  let key_fn = Fun.id in
  let metrics = make_state metrics_id in
  call_and_remember metrics mtx ht key_fn f x
