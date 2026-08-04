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
(* Tests for our Domain module, and for various operations that
 * rely on domain-local state. *)
module H = Hook

let t = Testo.create
let exnt = Alcotest.testable Fmt.exn ( = )

let timeout : [ `Timeout ] Alcotest.testable =
  Alcotest.testable (fun pff _ -> Format.fprintf pff "`Timeout") ( = )

let conf_or_die env =
  match Parallelism_config.create env with
  | Parallelism_config.Eio_executor conf -> conf
  | _ ->
      Alcotest.fail
        "Failed to get a Parallelism_config.Eio_executor from a \
         Parallelism_config.create"

(* Ensures that when new Domains are spawned, the assigned value
 * is read from the parent. *)
let test_hook_inherit_val () =
  let h = H.create 99 in

  (* Confirm that [with_hook_set] scopes the value of h. *)
  let n = H.with_hook_set h 1 (fun () -> H.get h) in
  Alcotest.(check int) __LOC__ 99 (H.get h);
  Alcotest.(check int) __LOC__ 1 n;

  (* Spawn a domain and then set *)
  let n =
    (fun () -> H.with_hook_set h 1 (fun () -> H.get h))
    |> Domain.spawn |> Domain.join
  in
  Alcotest.(check int) __LOC__ 99 (H.get h);
  Alcotest.(check int) __LOC__ 1 n;

  (* Set and then spawn a domain *)
  let n =
    H.with_hook_set h 1 (fun () ->
        (fun () -> H.get h) |> Domain.spawn |> Domain.join)
  in
  Alcotest.(check int) __LOC__ 99 (H.get h);
  Alcotest.(check int) __LOC__ 1 n

(* Ensures that Domains.map plays well with hooked per-fiber values. *)
let test_fiber_local_concurrent_map () =
  let h = H.create 0 in
  let procs = 4 in

  (* This will repeatedly check that binding [sm]'s value to [i]
   * is not disturbed by another fiber nor another domain. *)
  let f i =
    assert (H.get h = 0);
    H.with_hook_set h i (fun () ->
        for _ = 0 to 1000 do
          let i' = H.get h in
          assert (i = i');
          Eio.Fiber.yield ()
        done);
    assert (H.get h = 0)
  in

  Eio_main.run @@ fun env ->
  let conf = conf_or_die env in

  let l = List.init procs (fun i -> i + 1) in
  let res = Concurrent.map ~conf ~domain_count:2 f l in

  assert (Result.is_ok (Result_.collect res));
  Alcotest.(check int) __LOC__ 0 (H.get h)

type test_t = { x : int; y : string }

let search ~term str =
  try Some (Str.search_forward (Str.regexp_string term) str 0) with
  | Not_found -> None

let contains ~term str = search ~term str <> None

(* Executor_pool.ml is like the eio executor pool but with the property that if
   an exception happens not on the normal stack and is not caught, it resolves
   the promise and restarts the domain, continuing on with more work.

   One example of where this can happen an asynchronous exception being raised
   in effect code, e.g. a gc alarm in eio scheduling code.

   [gc_alarm_bomb] never returns normally: it loops until a GC alarm fires
   inside eio scheduling code, killing the worker that way. Since that is the
   only way to provoke such an exception from a test, it is also the only way
   to reach [Concurrent.map]'s chunk-poisoning branch. *)
let gc_alarm_bomb i =
  (* Use atomic to ensure that we only run the gc alarm in code we can recover
     and retry on*)
  let alarm_running = Atomic.make false in
  let _ =
    Gc.create_alarm (fun () ->
        if Atomic.get alarm_running then
          raise (Failure (Printf.sprintf "GC Alarm triggered: %d" i)))
  in
  (* Functon that will allocate some stuff while yielding a lot in hopes we
     trigger a gc alarm in eio scheduling code *)
  let rec f' () =
    try
      Atomic.set alarm_running true;
      let random_list =
        List.init 1000 (fun _ ->
            Eio.Fiber.yield ();
            { x = Random.int 100000; y = string_of_int (Random.int 100000) })
      in
      let _sorted = List.sort compare random_list in
      Eio.Fiber.yield ();
      Atomic.set alarm_running false;
      (* if we reach here we haven't triggered the gc alarm so let's retry *)
      f' ()
    with
    | Failure _ ->
        (* If we reach here we HAVE triggered the gc alarm but not in eio
         scheduling code so retry *)
        Atomic.set alarm_running false;
        f' ()
  in
  f' ()

let test_concurrent_map_async_exception () =
  let f = gc_alarm_bomb in
  Eio_main.run @@ fun env ->
  let conf = conf_or_die env in

  (* Run 3 jobs on 2 domains to ensure that domains restart *)
  let l = List.init 3 (fun i -> i + 1) in
  let res = Concurrent.map ~conf ~domain_count:2 f l in
  match Result_.collect res with
  | Ok _ -> Alcotest.fail "Expected exception but got Ok"
  | Error (_, e) when contains ~term:"GC Alarm triggered" (Printexc.to_string e)
    ->
      (* Make sure length of res is 3 exceptions, i.e. we actually restarted a
         domain and kept trying work *)
      (* assume that the rest of the exceptions are the same :shrug: *)
      let exns =
        (* nosemgrep: no-list-filter-map *)
        List.filter_map
          (function
            | Error e -> Some e
            | Ok _ -> None)
          res
      in
      Alcotest.(check int) "All 3 jobs returned exceptions" 3 (List.length exns)
  | Error (_, e) ->
      Alcotest.failf "Unexpected exception: %s" (Printexc.to_string e)

let test_concurrent_map_zero_domains () =
  Eio_main.run @@ fun env ->
  let conf = conf_or_die env in
  (* For nonsensical domain count arguments, override to 1. *)
  let res = Concurrent.map ~conf ~domain_count:0 (fun x -> x + 1) [ 1; 2; 3 ] in
  Alcotest.(check int) "Mapping operation total" 3 (List.length res)

let test_concurrent_map_empty_list () =
  Eio_main.run @@ fun env ->
  let conf = conf_or_die env in
  let res = Concurrent.map ~conf ~domain_count:2 (fun x -> x + 1) [] in
  Alcotest.(check int) "empty list returns empty result" 0 (List.length res)

let test_concurrent_map_order () =
  Eio_main.run @@ fun env ->
  let conf = conf_or_die env in
  let clock = Eio.Stdenv.clock env in
  let jobs_1 = [ 0.3; 0.03; 0.03; 0.03 ] in
  let jobs_2 = [ 0.03; 0.03; 0.03; 0.3 ] in
  let f jobs =
    Concurrent.map ~conf ~domain_count:2
      (fun timeout ->
        Eio.Time.sleep clock timeout;
        timeout)
      jobs
    |> Result_.collect |> Result.get_ok
  in
  Alcotest.(check (list (float 0.001)))
    "preserve order (slowest first)" jobs_1 (f jobs_1);
  Alcotest.(check (list (float 0.001)))
    "preserve order (slowest last)" jobs_2 (f jobs_2)

(* Inputs large enough to be split into many chunks (including a ragged final
   chunk) must still map every element exactly once, in order. *)
let test_concurrent_map_chunked_order () =
  Eio_main.run @@ fun env ->
  let conf = conf_or_die env in
  let l = List.init 10_000 Fun.id in
  let res =
    Concurrent.map ~element_cost:`Cheap_uniform ~conf ~domain_count:2
      (fun x -> x * 2)
      l
    |> Result_.collect |> Result.get_ok
  in
  (* 10_000 elements over 2 domains sizes chunks at the 256 cap, so this is 39
     full chunks plus a ragged 16-element tail. *)
  Alcotest.(check int)
    "input is actually chunked" 256
    (Concurrent.uniform_chunk_size ~domain_count:2 l);
  Alcotest.(check (list int))
    "chunked map preserves order and completeness"
    (List.map (fun x -> x * 2) l)
    res

(* An exception raised by [f] must be attributed to exactly the element that
   raised it, not to its whole chunk. *)
let test_concurrent_map_chunked_error_granularity () =
  Eio_main.run @@ fun env ->
  let conf = conf_or_die env in
  let bad = 7_777 in
  let l = List.init 10_000 Fun.id in
  let res =
    Concurrent.map ~element_cost:`Cheap_uniform ~conf ~domain_count:2
      (fun x -> if x = bad then failwith "boom" else x)
      l
  in
  Alcotest.(check int) "one result per element" 10_000 (List.length res);
  List.iteri
    (fun i r ->
      match r with
      | Ok x -> Alcotest.(check int) "ok element unchanged" i x
      | Error (x, exn) ->
          Alcotest.(check int) "only the raising element errors" bad x;
          Alcotest.(check int) "error at its own position" bad i;
          Alcotest.(check exnt) "error payload" (Failure "boom") exn)
    res

(* The sizing policy must never chunk small inputs: n tasks on n domains
   must yield chunk size 1 (full parallelism), and chunking only engages once
   there are at least eight chunks per domain. *)
let test_uniform_chunk_size () =
  let l n = List.init n Fun.id in
  Alcotest.(check int)
    "8 tasks on 8 domains stay per-element" 1
    (Concurrent.uniform_chunk_size ~domain_count:8 (l 8));
  (* Below a quotient of 2 the policy yields 1; at domain_count 8 that
     threshold is 16 * 8 = 128 elements, so 127 truncates to 1. *)
  Alcotest.(check int)
    "below the 16 * domain_count threshold stays per-element" 1
    (Concurrent.uniform_chunk_size ~domain_count:8 (l 127));
  Alcotest.(check int)
    "large inputs cap at 256" Concurrent.max_chunk_size
    (Concurrent.uniform_chunk_size ~domain_count:7 (l 250_000));
  (* Deliberately below saturation: at 10_000/4 the formula would return the
     256 cap, and ceil(10000/256) = 40 >= 32 holds for *any* target factor, so
     that input cannot falsify the property. At 1_000/4 the chunk size is 31,
     giving 33 chunks per the 32 required — and dropping the target factor to
     4 would yield 62 and only 17 chunks, failing as it should. *)
  let domain_count = 4 and length = 1_000 in
  let cs = Concurrent.uniform_chunk_size ~domain_count (l length) in
  Alcotest.(check bool) "chunk size is below the cap" true (cs < 256);
  Alcotest.(check bool)
    "at least 8 chunks per domain" true
    ((length + cs - 1) / cs >= 8 * domain_count);
  (* Nonsensical domain counts are treated as 1, matching [map]. *)
  Alcotest.(check int)
    "domain_count 0 tolerated" 1
    (Concurrent.uniform_chunk_size ~domain_count:0 (l 3));
  Alcotest.(check int)
    "negative domain_count tolerated" 1
    (Concurrent.uniform_chunk_size ~domain_count:(-1) (l 3));
  (* Absurd domain counts must be clamped rather than overflowing the
     divisor. Unclamped, [domain_count * min_chunks_per_domain] wraps: at
     multiples of 2^60 it lands on exactly 0 and the division raises, and at
     max_int it goes negative and the traversal cap returns immediately,
     yielding the 256 cap on a 3-element list instead of 1. *)
  List.iter
    (fun domain_count ->
      Alcotest.(check int)
        (Printf.sprintf "absurd domain_count %d clamped" domain_count)
        1
        (Concurrent.uniform_chunk_size ~domain_count (l 3)))
    [ 1 lsl 60; 1 lsl 61; 1 lsl 62; max_int ];
  (* The exact saturation boundary: the traversal cap and the formula agree.
     The 8 here is [min_chunks_per_domain], which is not exported — if that
     constant changes, these two assertions fail under names that no longer
     describe a boundary. *)
  let sat = 7 * 8 * Concurrent.max_chunk_size in
  Alcotest.(check int)
    "chunk size at the saturation boundary" Concurrent.max_chunk_size
    (Concurrent.uniform_chunk_size ~domain_count:7 (l sat));
  Alcotest.(check int)
    "chunk size just below the saturation boundary"
    (Concurrent.max_chunk_size - 1)
    (Concurrent.uniform_chunk_size ~domain_count:7 (l (sat - 56)))

(* [element_cost] is a performance hint, so it must not be observable in the
   results: both settings map every element exactly once, in order. This is
   the contract that lets [`Coarse] and [`Cheap_uniform] share one code path.
   Sizes span the chunking threshold — 3_000 elements over 2 domains chunks,
   10 elements does not (below [16 * domain_count] the policy yields 1). *)
let test_concurrent_map_cost_equivalence () =
  Eio_main.run @@ fun env ->
  let conf = conf_or_die env in
  let run element_cost l =
    Concurrent.map ~element_cost ~conf ~domain_count:2 (fun x -> x + 1) l
    |> Result_.collect |> Result.get_ok
  in
  List.iter
    (fun n ->
      let l = List.init n Fun.id in
      let expected = List.map (fun x -> x + 1) l in
      Alcotest.(check (list int))
        (Printf.sprintf "coarse, %d elements" n)
        expected (run `Coarse l);
      Alcotest.(check (list int))
        (Printf.sprintf "cheap_uniform, %d elements" n)
        expected (run `Cheap_uniform l))
    [ 0; 1; 10; 3_000 ]

(* Within a chunk, [f]'s side effects must happen in list order. *)
let test_concurrent_map_chunked_effect_order () =
  Eio_main.run @@ fun env ->
  let conf = conf_or_die env in
  let seen = ref [] in
  let l = List.init 100 Fun.id in
  let res =
    Concurrent.map ~element_cost:`Cheap_uniform ~conf ~domain_count:1
      (fun x ->
        seen := x :: !seen;
        x)
      l
    |> Result_.collect |> Result.get_ok
  in
  Alcotest.(check (list int)) "results in order" l res;
  Alcotest.(check (list int)) "effects in list order" l (List.rev !seen)

(* [Cancelled] raised by [f] itself must come back out of [map] rather than
   degrade into per-element [Error]s. This covers one specific route: the
   per-element handler re-raises, the pool's task wrapper converts it to
   [Error Cancelled], and [map]'s chunk handler must re-raise rather than
   fabricate results. A real external cancel does *not* take this route (see
   [test_concurrent_map_cancel_context]), so this is named for what it
   covers.

   Both [element_cost] settings are checked: before the two submission paths
   were unified, only the batched one re-raised, and the default path — the
   one every other caller uses — turned cancellation into a per-element
   [Error]. *)
let test_concurrent_map_reraises_cancelled_from_f () =
  Eio_main.run @@ fun env ->
  let conf = conf_or_die env in
  let l = List.init 1_000 Fun.id in
  List.iter
    (fun element_cost ->
      match
        Concurrent.map ~element_cost ~conf ~domain_count:2
          (fun x -> if x = 500 then raise (Eio.Cancel.Cancelled Exit) else x)
          l
      with
      | _results -> Alcotest.fail "expected Cancelled to propagate"
      | exception Eio.Cancel.Cancelled _ -> ())
    [ `Coarse; `Cheap_uniform ]

(* A genuine external cancel takes a different route than [f] raising
   [Cancelled]: [Executor_pool.submit] is [enqueue |> Promise.await], so the
   cancel fires in the awaiting fiber and never reaches [map]'s chunk
   handler at all. This pins the claim that removing the per-element
   [Eio.Fiber.check] left cancellation working — a batched map whose cancel
   context is cancelled must not run to completion. *)
let test_concurrent_map_cancel_context () =
  Eio_main.run @@ fun env ->
  let conf = conf_or_die env in
  let clock = Eio.Stdenv.clock env in
  let l = List.init 1_000 Fun.id in
  let completed = ref false in
  let result =
    Eio.Time.with_timeout clock 0.1 (fun () ->
        let res =
          Concurrent.map ~element_cost:`Cheap_uniform ~conf ~domain_count:2
            (fun x ->
              Eio.Time.sleep clock 0.05;
              x)
            l
        in
        completed := true;
        Ok res)
  in
  Alcotest.(check timeout)
    "cancelled before completing" `Timeout
    (match result with
    | Error `Timeout -> `Timeout
    | Ok _ -> Alcotest.fail "expected the map to be cancelled, not to finish");
  Alcotest.(check bool) "map did not run to completion" false !completed

(* A worker killed mid-chunk poisons its whole chunk, and the results for
   that chunk are *reconstructed* from the (suffix, count) spec rather than
   produced by mapping — the one place in [map] where that happens. A wrong
   count or suffix would silently return fewer results than input elements,
   and callers like [Find_targets.filter_paths] consume results positionally,
   so targets would vanish from a scan with nothing reported. *)
let test_concurrent_map_chunk_poisoning () =
  Eio_main.run @@ fun env ->
  let conf = conf_or_die env in
  (* 16 elements over 1 domain sizes chunks at 2, so poisoning has to cover
     more than the element that was running. *)
  let n = 16 in
  let l = List.init n Fun.id in
  Alcotest.(check int)
    "input is actually chunked" 2
    (Concurrent.uniform_chunk_size ~domain_count:1 l);
  let res =
    Concurrent.map ~element_cost:`Cheap_uniform ~conf ~domain_count:1
      gc_alarm_bomb l
  in
  Alcotest.(check int)
    "one result per element even when chunks die" n (List.length res);
  (* Every element is attributed to itself, in position — this is what pins
     the spec reconstruction. *)
  List.iteri
    (fun i r ->
      match r with
      | Ok _ -> Alcotest.failf "element %d unexpectedly succeeded" i
      | Error (x, e) ->
          Alcotest.(check int) "error attributed to its own element" i x;
          Alcotest.(check bool)
            (Printf.sprintf "element %d died from the GC alarm" i)
            true
            (contains ~term:"GC Alarm triggered" (Printexc.to_string e)))
    res

let tests =
  Testo.categorize "Concurrent"
    [
      t "test_hook_inherit_val" test_hook_inherit_val;
      t "Fiber with Concurrent.map" test_fiber_local_concurrent_map;
      t "test_concurrent_map_async_exception"
        test_concurrent_map_async_exception;
      t "Concurrent.map with zero domains" test_concurrent_map_zero_domains;
      t "Concurrent.map on empty list" test_concurrent_map_empty_list;
      t "Concurrent.map preserves order" test_concurrent_map_order;
      t "uniform_chunk_size policy" test_uniform_chunk_size;
      t "Concurrent.map chunked order" test_concurrent_map_chunked_order;
      t "Concurrent.map chunked error granularity"
        test_concurrent_map_chunked_error_granularity;
      t "Concurrent.map element_cost does not change results"
        test_concurrent_map_cost_equivalence;
      t "Concurrent.map chunked effect order"
        test_concurrent_map_chunked_effect_order;
      t "Concurrent.map re-raises Cancelled surfaced by f"
        test_concurrent_map_reraises_cancelled_from_f;
      t "Concurrent.map honours a cancelled cancel context"
        test_concurrent_map_cancel_context;
      t "Concurrent.map chunk poisoning on worker death"
        test_concurrent_map_chunk_poisoning;
    ]
