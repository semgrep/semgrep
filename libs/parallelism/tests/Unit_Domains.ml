(* Tests for our Domain module, and for various operations that
 * rely on domain-local state. *)

let t = Testo.create
let exnt = Alcotest.testable Fmt.exn ( = )

let timeout : Alcotest.([ `Timeout ] testable) =
  Alcotest.testable (fun pff _ -> Format.fprintf pff "`Timeout") ( = )

(* Ensures that when new Domains are spawned, the assigned value
 * is read from the parent. *)
let test_hook_inherit_val () =
  let h = Hook.create 0 in

  (* Confirm that `with_hook_set` scopes the value of h. *)
  Hook.with_hook_set h 1 (fun () -> Alcotest.(check int) __LOC__ (Hook.get h) 1);
  Alcotest.(check int) __LOC__ (Hook.get h) 0;

  (* Confirm that when a domain is spawned, we inherit the value
   * that has been set by the hook versus re-running the initialization
   * function. *)
  let n =
    Hook.with_hook_set h 42 (fun () ->
        Domain.join (Domain.spawn (fun () -> Hook.get h)))
  in
  Alcotest.(check int) __LOC__ n 42

(* Ensures that Domains.map plays well with hooked per-fiber values. *)
let test_fiber_local_domains_map () =
  let module H = Hook in
  let h = H.create 0 in
  let procs = 4 in

  (* This will repeatedly check that binding [sm]'s value to [i]
   * is not disturbed by another fiber nor another domain. *)
  let f i =
    assert (H.get h = 0);
    H.with_hook_set h i (fun () ->
        for _ = 0 to 1000 do
          let i' = H.get h in
          Eio.Fiber.yield ();
          assert (i = i');
          (* See above comment about races in the test harness *)
          Eio.Fiber.yield ()
        done);
    assert (H.get h = 0)
  in

  Eio_main.run (fun env ->
      Eio.Switch.run (fun sw ->
          let dm = Eio.Stdenv.domain_mgr env in
          let pool = Eio.Executor_pool.create ~sw ~domain_count:procs dm in

          let l = List.init procs (fun i -> i + 1) in
          let res = Domains.map ~pool f l in
          assert (Result.is_ok (Result_.collect res))));
  Alcotest.(check int) __LOC__ 0 (H.get h)

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
  let f = Domains.wrap_timeout_exn ~clock 0.1 forty_two in
  let res = result_of_exn f in
  Alcotest.(check (result int exnt)) __LOC__ (res ()) (Error Eio.Time.Timeout);

  (* Do not exceed our timeout deadline !*)
  let f = Domains.wrap_timeout_exn ~clock 0.5 forty_two in
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
  let f = Domains.wrap_timeout ~clock 0.1 forty_two in
  let res = f () in
  Alcotest.(check (result int timeout)) __LOC__ res (Error `Timeout);

  (* Do not exceed our timeout deadline !*)
  let f = Domains.wrap_timeout ~clock 0.5 forty_two in
  Alcotest.(check (result int timeout)) __LOC__ (f ()) (Ok 42)

let test_domain_map_timeouts () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let pool =
    Eio.Executor_pool.create ~sw (Eio.Stdenv.domain_mgr env) ~domain_count:2
  in

  let sleep s =
    Eio.Time.sleep clock s;
    s
  in

  (* The happy case: no Ensure we handle no timeouts. *)
  let xs = [ 0.1; 0.2; 0.1; 0.2 ] in
  let res = Domains.map pool (Domains.wrap_timeout_exn ~clock 0.5 sleep) xs in
  Alcotest.(check (list (result (float 0.001) exnt)))
    __LOC__ res
    [ Ok 0.1; Ok 0.2; Ok 0.1; Ok 0.2 ];

  (* An unhappy case: Some timeouts. *)
  let xs = [ 0.1; 0.7; 0.2; 0.7 ] in
  let res = Domains.map pool (Domains.wrap_timeout_exn ~clock 0.5 sleep) xs in
  Alcotest.(check (list (result (float 0.001) exnt)))
    __LOC__ res
    [ Ok 0.1; Error Eio.Time.Timeout; Ok 0.2; Error Eio.Time.Timeout ]

let test_burn () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in

  (* Unlike the operation that sleeps, as above, this is "pure computation" and
   * does not call back into the Eio runtime automatically, unless we manually
   * do so, such as by calling [Eio.Fiber.yield ()] or writing to a Flow. *)
  (* let out = Eio_mock.Flow.make "Logger" in *)
  let out = Eio.Stdenv.stdout env in
  let burn () =
    while true do
      for _ = 0 to 10000000 do
        (* NB: this is _not_ the same thing as Eio.Time.Sleep, as sleeping is
         * performing an Effect; busywaiting in this way is _not_. *)
        ()
      done;
      Eio.Flow.copy_string "Thinking...\n" out
    done
  in

  (* Exceed our timeout deadline. *)
  let f = Domains.wrap_timeout ~clock 0.1 burn in
  let res = f () in
  Alcotest.(check (result int timeout)) __LOC__ res (Error `Timeout)

let tests =
  Testo.categorize "Domains"
    [
      t "test_hook_inherit_val" test_hook_inherit_val;
      t "Fiber with Domains.map" test_fiber_local_domains_map;
      t "test_wrap_timeout_exn" test_wrap_timeout_exn;
      t "test_wrap_timeout" test_wrap_timeout;
      t "test_domain_map_timeouts" test_domain_map_timeouts;
      t "test_burn" test_burn;
    ]
