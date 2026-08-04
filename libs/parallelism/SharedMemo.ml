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
   safe current access to the memoized state.

   The cache is wrapped in an opaque [t] so that the representation (today
   a single [Mutex.t] + [Hashtbl.t]) can evolve e.g., to a striped /
   sharded table, without changing the API. *)

(*****************************************************************************)
(* Cache type *)
(*****************************************************************************)

type ('k, 'v) t = { mtx : Mutex.t; ht : ('k, 'v) Hashtbl.t }

let create ?(initial_size = 101) () =
  { mtx = Mutex.create (); ht = Hashtbl.create ~random:true initial_size }

let remove t k = Mutex.protect t.mtx (fun () -> Hashtbl.remove t.ht k)
let length t = Mutex.protect t.mtx (fun () -> Hashtbl.length t.ht)

let iter f t =
  (* Snapshot under the lock, then iterate outside it.  This keeps the lock
     hold time bounded and, crucially, lets [f] safely call back into the
     same [t] (e.g. via a memoizer built on it) without deadlocking.

     The snapshot isn't atomic with respect to writers that run after the
     snapshot is taken: under a future sharded backing, snapshotting
     shard-by-shard preserves exactly this contract.
   *)
  let snapshot =
    Mutex.protect t.mtx (fun () ->
        Hashtbl.fold (fun k v acc -> (k, v) :: acc) t.ht [])
  in
  List.iter (fun (k, v) -> f k v) snapshot

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let call_and_remember ?(should_cache = Fun.const true) t key_fn f k =
  (* [should_cache v] decides whether [v] is inserted into [ht]. The default
   * [fun _ -> true] reproduces unconditional caching. Passing a predicate
   * lets specialized entry points (e.g. [make_result_with_state]) skip
   * caching for values that embed caller-specific state, such as [Error]s
   * carrying source locations. *)
  let k' = key_fn k in
  (* Assumption: [k] will more often than not be a cache hit.  As a result,
   * in the unlikely event of a cache miss, we pay the cost of unlocking and
   * relocking to insert the new kv pair. *)
  match Mutex.protect t.mtx (fun () -> Hashtbl.find_opt t.ht k') with
  | Some v -> v
  | None ->
      (* Note: f could well be an expensive computation, so do not starve
       other accesses to the hashtable while we call it by holding the mutex.

       This does leave open the possibility that two threads will race on
       computation on the same key: whoever gets there first will not be
       overridden by the straggler.  Since f has to be deterministic, this
       is fine (and while unfortunate, still preferable to holding the lock
       through the computation.
     *)
      let v = f k in
      if not (should_cache v) then v
      else
        Mutex.protect t.mtx (fun () ->
            match Hashtbl.find_opt t.ht k' with
            (* Someone beat us to the insert! So it goes; discard our copy. *)
            | Some v' -> v'
            | None ->
                Hashtbl.add t.ht k' v;
                v)

let make_with_state ?should_cache t =
  let key_fn = Fun.id in
  call_and_remember ?should_cache t key_fn

let make_with_key_fn ?should_cache key_fn =
  let t = create () in
  call_and_remember ?should_cache t key_fn

let make_with_state_legacy mtx ht =
  let key_fn = Fun.id in
  call_and_remember { mtx; ht } key_fn

(* eta-expand just enough to make the binding a lambda; the returned
   function (e.g. the rhs of `let memo_fn = make fn`) holds onto the `.t`, but
   only evaluates `make_with_key_fn Fun.id` once, at memo construction time.

   Originally, this was `let make f x = ...`, which pulls `make_with_key_fn
   Fun.id` _inside_ the lambda, so it's re-evaluated on each call; this means
   we rebuild the cache each time.

   Later, an attempt to "correct" this to `let make = make_with_key_fn Fun.id`
   was made, which had the opposite problem: `make_with_key_fun` is only ever
   called once, no matter how many `SharedMemo`s are created, and so one `.t`
   is shared across _all_ memos!  (Luckily, the value restriction, combined
   with the explicit signature in the interface, causes this to not compile.)
*)
let make ?should_cache f = make_with_key_fn ?should_cache Fun.id f
