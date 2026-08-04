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

(** [capped_length n l] returns [min (List.length l) n] without traversing
    more than [n] elements of [l]. *)
let capped_length n l =
  let rec aux i = function
    | _ when i >= n -> n
    | [] -> i
    | _ :: tl -> aux (i + 1) tl
  in
  aux 0 l

(* Every [Executor_pool.submit] costs two cross-domain thread wakeups plus a
   fresh fiber on the worker. For microsecond-scale elements that overhead
   dwarfs the work itself, so callers with many cheap, similarly-priced
   elements can ask for elements to be submitted in chunks.

   Chunking trades load balance for overhead: elements sharing a chunk are
   serialized on one domain, so a chunk containing several expensive elements
   becomes a straggler. It is therefore opt-in, and a caller opts in by
   describing its elements rather than by supplying a size: sizing needs the
   domain count and the input, both of which [map] already has, so keeping
   the policy here means there is no second copy of those inputs to disagree
   with.

   A chunk is represented as a suffix of the input list plus an element count:
   lists are immutable, so the suffix shares structure with the input and
   chunking allocates one pair per chunk rather than re-consing elements into
   chunk lists. Reading the shared suffix from a worker domain is safe for the
   same reason. *)

let max_chunk_size = 256

(* Aiming for several chunks per domain keeps domains load balanced when
   element costs are uneven. *)
let min_chunks_per_domain = 8

(* [divisor] below overflows for absurd domain counts, and wraps to exactly 0
   for multiples of 2^60, which would make the division raise. No real
   machine has this many domains, so clamping costs nothing. *)
let max_domain_count = 65_536

(* The chunk-size formula saturates at [max_chunk_size] once the input
   reaches [domain_count * min_chunks_per_domain * max_chunk_size] elements,
   so [capped_length] up to that point determines the result exactly without
   traversing the rest of the input. *)
let uniform_chunk_size ~domain_count l =
  (* Tolerate nonsensical domain counts the same way [map] does. *)
  let domain_count = max 1 (min domain_count max_domain_count) in
  let divisor = domain_count * min_chunks_per_domain in
  let n = capped_length (divisor * max_chunk_size) l in
  max 1 (min max_chunk_size (n / divisor))

(* Single pass: visits each cell of [l] exactly once, without needing the
   total length. Requires [chunk_size >= 1]; [map] guarantees this. *)
let chunk_specs ~chunk_size l =
  let rec advance n l =
    if n = 0 then (0, l)
    else
      match l with
      | [] -> (n, [])
      | _ :: tl -> advance (n - 1) tl
  in
  let rec aux l acc =
    match l with
    | [] -> List.rev acc
    | _ ->
        let unconsumed, rest = advance chunk_size l in
        aux rest ((l, chunk_size - unconsumed) :: acc)
  in
  aux l []

(* [map_first f n l] maps [f] over the first [n] elements of [l], applying
   [f] in list order (the [let] pins evaluation order; a bare
   [f x :: map_first ...] would run the recursive call first). Plain non-tail
   recursion is fine here: [n] is a chunk size, and every chunk size comes
   either from [uniform_chunk_size], which caps at [max_chunk_size], or from
   the literal 1. *)
let rec map_first f n l =
  if n = 0 then []
  else
    match l with
    | [] -> []
    | x :: tl ->
        let y = f x in
        y :: map_first f (n - 1) tl

let map ?(element_cost : [ `Coarse | `Cheap_uniform ] = `Coarse)
    ~(conf : Parallelism_config.eio_state) ~domain_count f l =
  Eio.Switch.run @@ fun sw ->
  let domain_mgr = Eio.Stdenv.domain_mgr conf.env in

  (* [`Coarse] is chunks of one, which is exactly the per-element submission
     every caller used before chunking existed: one pool task per element,
     [map_first] applied to a single element, and a poisoned chunk covering
     just that element. So there is one code path below rather than two. *)
  let chunk_size =
    match element_cost with
    | `Coarse -> 1
    | `Cheap_uniform -> uniform_chunk_size ~domain_count l
  in
  let specs = chunk_specs ~chunk_size l in

  (* NOTE: [submit] blocks the fiber until the task returns a result.*)
  (* Please see the comment block in [Hook.ml] concerning safe values of
   * [weight], if you are intending on changing it! A chunk is a single pool
   * task, so it occupies a whole domain while it runs, just as a single
   * element does when [chunk_size] is 1. *)
  (* A chunk is the unit of work here, so cap domains by the number of
     chunks, not the number of elements. *)
  let domain_count = max 1 (capped_length domain_count specs) in
  let pool = Executor_pool.create ~sw ~domain_count domain_mgr in

  (* nosemgrep: no-logs-in-library *)
  Logs.debug (fun m ->
      m "Mapping %d elements across %d domains (%d chunks of <= %d)"
        (List.length l) domain_count (List.length specs) chunk_size);

  Eio.Fiber.List.map ~max_fibers:domain_count
    (fun (suffix, n) ->
      match
        Executor_pool.submit pool ~weight:1.0 (fun () ->
            (* No per-element cancellation poll: cross-domain cancellation
               is delivered by this domain's scheduler, which cannot run
               while the chunk executes, so a poll here could never
               observe it. Cancellation lands between chunks (a
               suspension point) or, when [f] itself performs Eio
               operations, inside [f] — which the handler below
               propagates rather than recording as an element failure. *)
            map_first
              (fun x ->
                try Ok (f x) with
                | Eio.Cancel.Cancelled _ as e ->
                    let bt = Printexc.get_raw_backtrace () in
                    Printexc.raise_with_backtrace e bt
                | e -> Error (x, e))
              n suffix)
      with
      | Ok results -> results
      | Error (Eio.Cancel.Cancelled _ as e) ->
          (* The worker's task wrapper converts our cancellation re-raise
             back into a value; propagate it here so a cancelled map
             raises instead of fabricating per-element failures for the
             chunk. *)
          raise e
      | Error err ->
          (* An asynchronous exception killed the worker mid-chunk; we
             cannot tell which element was running, so attribute the
             failure to every element of the chunk, including elements
             that may already have completed. *)
          map_first (fun x -> Error (x, err)) n suffix)
    specs
  (* Tail recursive: the chunk count grows with the input, so [List.concat]
     and [List.concat_map] would put it on the stack. *)
  |> List_.flatten
[@@tracing]
