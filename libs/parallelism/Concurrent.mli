(* Cooper Pierce and Yosef Alsuhaibani
 *
 * Copyright (C) Semgrep, Inc. All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see
 * <https://www.gnu.org/licenses/>.
 *)

val max_chunk_size : int
(** Upper bound on the chunk sizes [uniform_chunk_size] produces, and so on
    the number of elements a single worker failure can take down with it (see
    [map]). Part of the sizing policy rather than of [map]'s interface: no
    caller supplies a chunk size. *)

val uniform_chunk_size : domain_count:int -> 'a list -> int
(** [uniform_chunk_size ~domain_count l] is the chunk size [map] uses for
    [`Cheap_uniform] elements. It targets at least eight chunks per domain (so
    domains stay load balanced) and caps chunks at [max_chunk_size] elements.
    Chunking only engages once [l] has at least [16 * domain_count] elements
    (the formula needs a quotient of at least 2); smaller inputs yield 1, i.e.
    per-element submission. The formula saturates at the cap, so only a
    bounded prefix of [l] is traversed. Nonpositive [domain_count] is treated
    as 1, and absurdly large values are clamped, since the formula would
    otherwise overflow. Exposed for tests and for reasoning about the policy;
    [map] applies it itself. *)

val map :
  ?element_cost:[ `Coarse | `Cheap_uniform ] ->
  conf:Parallelism_config.eio_state ->
  domain_count:int ->
  ('a -> 'b) ->
  'a list ->
  ('b, 'a * exn) result list
(** [map ~conf ~domain_count f xs] applies [f] to every element of [xs] in
    parallel via [domain_count] domains spawned via [conf].

    Each element in [xs] corresponds to [Ok res] if [f elem] evaluates to [res]
    or [Err exn] if [f elem] raised the exception [exn].

    [element_cost] describes the work [f] does per element, and [map] picks a
    submission strategy from it. [`Coarse] (the default) submits one element
    per worker task, which maximizes load balance and is right for expensive
    elements such as whole-file work. Each submission costs two cross-domain
    thread wakeups, which dominates when elements are cheap, so
    [`Cheap_uniform] instead batches consecutive elements into one task, sized
    by [uniform_chunk_size]. It is only appropriate when element costs are
    both small and roughly uniform: elements sharing a chunk are serialized on
    one domain, so a chunk holding several expensive elements becomes a
    straggler.

    Elements sharing a chunk run serially, in list order, on one domain.

    Batching does not widen the blast radius of an ordinary failure. If [f x]
    raises, only [x] is reported as [Error]; every other element of its chunk
    still yields its own [Ok] or [Error]. [element_cost] only affects the two
    failure modes below.

    Cancellation propagates out of [map] as an exception rather than as
    per-element [Error]s. A chunk whose [f] performs no Eio operations is not
    interrupted mid-chunk: cancellation takes effect between chunks, which
    under the intended cheap-element usage bounds the added latency to
    milliseconds.

    A worker killed mid-chunk by an asynchronous exception — one raised
    outside the normal stack of [f], such as in a GC alarm, a memprof
    callback, or a signal handler — poisons its whole chunk: we cannot tell
    which element was running, so every element of that chunk is reported as
    [Error] with that exception, rather than just the one that was running.
    Under [`Coarse] a chunk is a single element, so this is invisible; under
    [`Cheap_uniform] it has two consequences for callers:

    - [Error] does not imply that [f] did not run. Elements of a poisoned
      chunk that had already completed successfully are reported as [Error]
      anyway, so any side effect [f] performed for them has still happened.
      An [f] whose result the caller simply discards on [Error] is safe; an
      [f] that registers side effects the caller expects to correspond to the
      [Ok]s it sees is not.
    - Poisoned elements are not retried, because [map] cannot know whether
      re-running [f] on them is safe. A caller that knows its own [f] is pure
      can retry the [Error]s itself. *)
