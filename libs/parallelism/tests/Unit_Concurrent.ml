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

(* Executor_pool.ml is like the eio executor pool but with the property that if
   an exception happens not on the normal stack and is not caught, it resolves
   the promise and restarts the domain, continuing on with more work.

   One example of where this can happen an asynchronous exception being raised
   in effect code, e.g. a gc alarm in eio scheduling code. *)
let test_concurrent_map_async_exception () =
  let f i =
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
  in
  Eio_main.run @@ fun env ->
  let conf = conf_or_die env in

  (* Run 3 jobs on 2 domains to ensure that domains restart *)
  let l = List.init 3 (fun i -> i + 1) in
  let res = Concurrent.map ~conf ~domain_count:2 f l in
  let search ~term str =
    try Some (Str.search_forward (Str.regexp_string term) str 0) with
    | Not_found -> None
  in

  let contains ~term str = search ~term str <> None in
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
    ]
