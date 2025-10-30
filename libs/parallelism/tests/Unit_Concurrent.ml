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
  let conf =
    match Parallelism_config.create env with
    | Parallelism_config.Eio_executor conf -> conf
    | _ ->
        Alcotest.fail
          "Failed to get a Parallelism_config.Eio_executor from a \
           Parallelism_config.create"
  in

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
  let conf =
    match Parallelism_config.create env with
    | Parallelism_config.Eio_executor conf -> conf
    | _ ->
        Alcotest.fail
          "Failed to get a Parallelism_config.Eio_executor from a \
           Parallelism_config.create"
  in
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
  | Error e when contains ~term:"GC Alarm triggered" (Printexc.to_string e) ->
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
  | Error e -> Alcotest.failf "Unexpected exception: %s" (Printexc.to_string e)

(* Ensures that we can set a deadline on a fiber with the exceptions-oriented API. *)
let test_wrap_timeout_exn () =
  let result_of_exn f () =
    try Ok (f ()) with
    | e -> Error e
  in
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let forty_two () =
    Eio.Time.sleep clock 0.25;
    42
  in

  (* Note: calling [f], our wrapped version of the function,
   * will run f on the current fiber.  An alternative is to
   * fork a new fiber on the current switch, thusly:
   *
   * Eio.Fiber.fork_promise sw f |> Eio.Promise.await_exn
   *)

  (* Exceed our timeout deadline. *)
  let f = Concurrent.wrap_timeout_exn ~clock 0.1 forty_two in
  let res = result_of_exn f in
  Alcotest.(check (result int exnt)) __LOC__ (res ()) (Error Eio.Time.Timeout);

  (* Do not exceed our timeout deadline !*)
  let f = Concurrent.wrap_timeout_exn ~clock 0.5 forty_two in
  let res = result_of_exn f in
  Alcotest.(check (result int exnt)) __LOC__ (res ()) (Ok 42)

(* Ensures that we can set a deadline on a fiber with the result-oriented API. *)
let test_wrap_timeout () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let forty_two () =
    Eio.Time.sleep clock 0.25;
    42
  in

  (* Note: calling [f], our wrapped version of the function,
   * will run f on the current fiber.  An alternative is to
   * fork a new fiber on the current switch, thusly:
   *
   * Eio.Fiber.fork_promise sw f |> Eio.Promise.await_exn
   *)

  (* Exceed our timeout deadline. *)
  let f = Concurrent.wrap_timeout ~clock 0.1 forty_two in
  let res = f () in
  Alcotest.(check (result int timeout)) __LOC__ res (Error `Timeout);

  (* Do not exceed our timeout deadline !*)
  let f = Concurrent.wrap_timeout ~clock 0.5 forty_two in
  Alcotest.(check (result int timeout)) __LOC__ (f ()) (Ok 42)

let test_concurrent_map_timeouts () =
  Eio_main.run @@ fun env ->
  let conf =
    match Parallelism_config.create env with
    | Parallelism_config.Eio_executor conf -> conf
    | _ -> raise Common.Impossible
  in
  let clock = Eio.Stdenv.clock env in

  let sleep s =
    Eio.Time.sleep clock s;
    s
  in

  (* The happy case: no Ensure we handle no timeouts. *)
  let xs = [ 0.1; 0.2; 0.1; 0.2 ] in
  let res =
    Concurrent.map ~conf ~domain_count:2
      (Concurrent.wrap_timeout_exn ~clock 0.5 sleep)
      xs
  in
  Alcotest.(check (list (result (float 0.001) exnt)))
    __LOC__ res
    [ Ok 0.1; Ok 0.2; Ok 0.1; Ok 0.2 ];

  (* An unhappy case: Some timeouts. *)
  let xs = [ 0.1; 0.7; 0.2; 0.7 ] in
  let res =
    Concurrent.map ~conf ~domain_count:2
      (Concurrent.wrap_timeout_exn ~clock 0.5 sleep)
      xs
  in
  Alcotest.(check (list (result (float 0.001) exnt)))
    __LOC__ res
    [ Ok 0.1; Error Eio.Time.Timeout; Ok 0.2; Error Eio.Time.Timeout ]

let test_burn () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in

  (* Unlike the operation that sleeps, as above, this is "pure computation" and
   * does not call back into the Eio runtime automatically, unless we manually
   * do so, such as by calling [Eio.Fiber.yield ()] or writing to a Flow. *)
  let burn () =
    while true do
      let i = ref 0 in
      for _ = 0 to 1000 do
        (* NB: this is _not_ the same thing as Eio.Time.Sleep, as sleeping is
         * performing an effect; busywaiting in this way is _not_. *)
        i := !i + 1
      done;
      Concurrent.maybe_yield ()
    done
  in

  (* Exceed our timeout deadline. *)
  let f = Concurrent.wrap_timeout ~clock 0.1 burn in
  let res = f () in
  Alcotest.(check (result int timeout)) __LOC__ res (Error `Timeout)

let tests =
  Testo.categorize "Concurrent"
    [
      t "test_hook_inherit_val" test_hook_inherit_val;
      t "Fiber with Concurrent.map" test_fiber_local_concurrent_map;
      t "test_wrap_timeout_exn" test_wrap_timeout_exn;
      t "test_wrap_timeout" test_wrap_timeout;
      t "test_concurrent_map_timeouts" test_concurrent_map_timeouts;
      t "test_burn" test_burn;
      t "test_concurrent_map_async_exception"
        test_concurrent_map_async_exception;
    ]
