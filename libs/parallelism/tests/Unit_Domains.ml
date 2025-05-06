(* Tests for our Domain module, and for various operations that
 * rely on domain-local state. *)

let t = Testo.create

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

let tests =
  Testo.categorize "Domains"
    [
      t "test_hook_inherit_val" test_hook_inherit_val;
      t "Fiber with Domains.map" test_fiber_local_domains_map;
    ]
