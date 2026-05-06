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
(* Ensures safe memoization of a function that can be called between threads.
 * If you are writing new code, think hard about whether your problem warrants
 * this solution!  This should typically only be used for legacy pre-multicore
 * shared state.
 *)

type ('k, 'v) t
(** Opaque cache backing a [SharedMemo].  The representation is deliberately
    hidden so the implementation can evolve (e.g., to a striped/sharded
    backing) without changing callers.  Construct one with [create] and
    pass it to [make_with_state]. *)

val create : ?initial_size:int -> unit -> ('k, 'v) t
(** Create an empty cache.  [initial_size] defaults to 101.  The underlying
    hashtable uses randomized hashing. *)

val remove : ('k, 'v) t -> 'k -> unit
(** Remove the binding for the given key, if present. *)

val length : ('k, 'v) t -> int
(** Number of bindings currently in the cache.  Under the current
    single-mutex backing this is exact; under a future sharded backing it
    is best-effort. *)

val iter : ('k -> 'v -> unit) -> ('k, 'v) t -> unit
(** Apply [f] to each (key, value) binding present at the point of
    iteration.

    [iter] takes a best-effort snapshot of the cache and applies [f] to
    each entry {i outside} the cache's lock.  Consequences:
    - concurrent writers may add or remove bindings without being
      reflected in this iteration;
    - [f] may safely call back into the same [t].

    Memory use is O(n) in the cache size at call time. *)

val make : ('a -> 'b) -> 'a -> 'b
(** Memoizes calls to the supplied function, such that reentrant calls across
   domains is safe.*)

val make_with_state : ('a, 'b) t -> ('a -> 'b) -> 'a -> 'b
(** Memoizes the given function for concurrent access, storing results in
    the supplied [t].  Use this when the cache needs to be reachable
    outside [make]'s scope (e.g. to call [remove] from a cleanup hook).

    {b A cache must back exactly one memoized function.}  Passing the same
    [t] to two [make_with_state] calls is unsupported: the two functions'
    entries will collide in the shared key space, producing silent
    wrong-answer bugs (one function returning another's cached value),
    cross-function invalidation via [remove], and mixed [iter]/[length]
    results.  The type system cannot rule this out because two functions
    with the same ['a -> 'b] signature unify onto the same [t]. *)

val make_with_key_fn : ('a -> 'k) -> ('a -> 'b) -> 'a -> 'b
(** Memoizes calls to the supplied function, such that reentrant calls across
   domains is safe.  The [key_fn] argument transforms the input argument for
   the memoizer.

   (For the curious: a more sensible API would make the key function an optional
   argument with the default being [Fun.id].  However, there's no way for the
   type system to unify ['a. 'a -> 'a] and ['a -> 'k].  If you had dependent
   types you could make the hashtable key type dependent on whether or not
   key_fn was provided, but sadly we don't have that.)
 *)

val make_with_state_legacy :
  Mutex.t -> ('a, 'b) Hashtbl.t -> ('a -> 'b) -> 'a -> 'b
(** Deprecated migration shim preserving the pre-[t] API that exposed the
    raw [Mutex.t] / [Hashtbl.t].  New code should use [make_with_state]
    with a cache from [create].  This shim will be removed once all
    in-tree callers have migrated. *)
