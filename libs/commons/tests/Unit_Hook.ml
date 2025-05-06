module H = Hook

let t = Testo.create

let test_fiber_local_with_hook_set_scope () =
  let h = H.create 0 in

  (* Confirm that `with_hook_set` scopes the value of h, even in the synchronous
   * world. *)
  H.with_hook_set h 1 (fun () ->
      let one = H.get h in
      Alcotest.(check int) __LOC__ one 1);
  Alcotest.(check int) __LOC__ (H.get h) 0;

  (* Ensure ordinary scoping works as expected with a single fiber. *)
  Eio_main.run (fun _env ->
      Alcotest.(check int) __LOC__ (H.get h) 0;
      H.with_hook_set h 1 (fun () ->
          let one = H.get h in
          Alcotest.(check int) __LOC__ one 1);
      Alcotest.(check int) __LOC__ (H.get h) 0)

let test_fiber_local_nested () =
  let h = H.create 0 in

  (* With a scoped value set in the synchronous world... *)
  H.with_hook_set h 1 (fun () ->
      Eio_main.run (fun _env ->
          (* First, confirm that we can read the sync-bound value. *)
          Alcotest.(check int) __LOC__ (H.get h) 1;

          (* Next, Bind a new value in effects.land. *)
          H.with_hook_set h 2 (fun () ->
              Alcotest.(check int) __LOC__ (H.get h) 2);

          (* Ensure our sync-bound value is undisturbed. *)
          Alcotest.(check int) __LOC__ (H.get h) 1));
  (* Lastly, ensure we have rolled back to the original state. *)
  Alcotest.(check int) __LOC__ (H.get h) 0

let test_fiber_local_concurrent () =
  let h = H.create 0 in

  (* This will repeatedly check that binding [sm]'s value to [i]
   * is not disturbed by another fiber. *)
  let f i =
    H.with_hook_set h i (fun () ->
        for _ = 0 to 1000 do
          let i' = H.get h in
          Eio.Fiber.yield ();
          Alcotest.(check int) __LOC__ i i';
          Eio.Fiber.yield ()
        done)
  in

  (* Now let's ramp up and try a whole bunch of fibers. *)
  Eio_main.run (fun _env ->
      let fibers = List.init 1000 (fun i -> fun () -> f i) in
      Eio.Fiber.all fibers)

let test_fiber_local_with_exn () =
  let h = H.create 0 in
  let msg = "A terrible fate has befallen this computation" in
  let f : unit -> unit = fun () -> failwith msg in

  (* Ensure that we correctly reset local state in the synchronous world if
   * an exception is raised. *)
  Alcotest.check_raises __LOC__ (Failure msg) (fun () -> H.with_hook_set h 1 f);
  Alcotest.(check int) __LOC__ 0 (H.get h);

  (* Now do the same in the fiber-world. *)
  Eio_main.run (fun _env ->
      Alcotest.check_raises __LOC__ (Failure msg) (fun () ->
          H.with_hook_set h 1 f);
      Alcotest.(check int) __LOC__ 0 (H.get h))

let test_cli_escape_hatch () =
  let hb = H.create false in
  let hi = H.create 42 in

  let argv = Array.of_list [ "myproc"; "-verbose"; "-nprocs=99" ] in
  let speclist =
    [
      ("-verbose", Hook.Arg.set hb, "Sets hb to true");
      ("-nprocs", Hook.Arg.int hi, "Sets hi to its value");
    ]
  in
  let current = ref 0 in
  let _ = Arg.parse_argv ~current argv speclist (fun _ -> ()) "..." in
  Alcotest.(check bool) __LOC__ true (H.get hb);
  Alcotest.(check int) __LOC__ 99 (H.get hi)

let tests =
  Testo.categorize "Hooks"
    [
      t "Fiber scope" test_fiber_local_with_hook_set_scope;
      t "Fiber nested" test_fiber_local_nested;
      t "Fiber concurrent" test_fiber_local_concurrent;
      t "Fiber and exceptions" test_fiber_local_with_exn;
      t "Fiber mutation by CLI parsing" test_cli_escape_hatch;
    ]
