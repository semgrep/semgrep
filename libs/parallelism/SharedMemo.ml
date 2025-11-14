(* Nathan Taylor
 *
 * Copyright (C) 2025 Semgrep Inc.
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
 * safe current access to the memoized state. *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let call_and_remember mtx ht key_fn f k =
  let k' = key_fn k in
  (* Assumption: [k] will more often than not be a cache hit.  As a result,
   * in the unlikely event of a cache miss, we pay the cost of unlocking and
   * relocking to insert the new kv pair. *)
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
      Mutex.protect mtx (fun () ->
          match Hashtbl.find_opt ht k' with
          (* Someone beat us to the insert! So it goes; discard our copy. *)
          | Some v' -> v'
          | None ->
              Hashtbl.add ht k' v;
              v)

let make_with_state mtx ht =
  let key_fn = Fun.id in
  call_and_remember mtx ht key_fn

let make_with_key_fn key_fn =
  let mtx = Mutex.create () in
  let ht = Hashtbl.create 101 in
  call_and_remember mtx ht key_fn

let make f x =
  let mtx = Mutex.create () in
  let ht = Hashtbl.create 101 in
  let key_fn = Fun.id in
  call_and_remember mtx ht key_fn f x
