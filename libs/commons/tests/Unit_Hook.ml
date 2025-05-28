module H = Hook

let t = Testo.create

let test_hook_local_with_hook_set_scope () =
  let h = H.create 0 in

  (* Ensure ordinary scoping works as expected with a single fiber. *)
  Alcotest.(check int) __LOC__ 0 (H.get h);
  H.with_hook_set h 1 (fun () -> Alcotest.(check int) __LOC__ 1 (H.get h));
  Alcotest.(check int) __LOC__ 0 (H.get h)

let test_hook_local_nested () =
  let h = H.create 0 in

  (* With a scoped value set... *)
  H.with_hook_set h 1 (fun () ->
      (* First, confirm that we can read the sync-bound value. *)
      Alcotest.(check int) __LOC__ 1 (H.get h);

      (* Next, Bind a new value. *)
      H.with_hook_set h 2 (fun () -> Alcotest.(check int) __LOC__ 2 (H.get h));

      (* Ensure our first bound value is undisturbed. *)
      Alcotest.(check int) __LOC__ 1 (H.get h));

  (* Lastly, ensure we have rolled back to the original state. *)
  Alcotest.(check int) __LOC__ 0 (H.get h)

let test_hook_local_concurrent () =
  (* This test depends on us running inside EIO, so we explicitly
   * set up the runtime here. *)
  Eio_main.run @@ fun _ ->
  let h = H.create 0 in

  (* This will repeatedly check that binding [sm]'s value to [i]
   * is not disturbed by another fiber. *)
  let f i =
    H.with_hook_set h i (fun () ->
        for _ = 0 to 100 do
          let i' = H.get h in
          Alcotest.(check int) __LOC__ i i';
          Eio.Fiber.yield ()
        done)
  in

  (* Now let's ramp up and try a whole bunch of fibers. *)
  let fibers = List.init 100 (fun i -> fun () -> f i) in
  Eio.Fiber.all fibers

let test_hook_local_with_exn () =
  let h = H.create 0 in
  let msg = "A terrible fate has befallen this computation" in
  let f : unit -> unit = fun () -> failwith msg in

  (* Ensure that we correctly reset local state in the synchronous world if
   * an exception is raised. *)
  Alcotest.check_raises __LOC__ (Failure msg) (fun () -> H.with_hook_set h 1 f);
  Alcotest.(check int) __LOC__ 0 (H.get h);

  (* Now do the same in the fiber-world. *)
  Alcotest.check_raises __LOC__ (Failure msg) (fun () -> H.with_hook_set h 1 f);
  Alcotest.(check int) __LOC__ 0 (H.get h)

let test_cli_unscoped_set () =
  let hb = H.create false in
  let hi = H.create 42 in

  let argv = Array.of_list [ "myproc"; "-verbose"; "-nprocs=99" ] in
  let speclist =
    [
      ("-verbose", Hook.Arg.set hb, "Sets hb to true");
      ("-nprocs", Hook.Arg.int hi, "Sets hi to its value");
    ]
  in
  (* Parse some arguments into [hb] and [hi]. *)
  let current = ref 0 in
  Arg.parse_argv ~current argv speclist (fun _ -> ()) "...";
  Alcotest.(check bool) __LOC__ true (H.get hb);
  Alcotest.(check int) __LOC__ 99 (H.get hi);

  (* Ensure that we can still perform scoped operations after an unscoped set. *)
  H.with_hook_set hi 555 (fun () ->
      Alcotest.(check int) __LOC__ 555 (H.get hi);

      (* Inside a scoped operation, we cannot perform an unscoped one !*)
      Alcotest.check_raises __LOC__
        (Failure "Must not call [unscoped_set] after [with_hook_set]")
        (fun () ->
          let current = ref 0 in
          Arg.parse_argv ~current argv speclist (fun _ -> ()) "..."));

  (* However, even though the scoped operation is complete, we are still forbidden from being
   * able to unconditionally set a Hook value! *)
  Alcotest.check_raises __LOC__
    (Failure "Must not call [unscoped_set] after [with_hook_set]") (fun () ->
      let current = ref 0 in
      Arg.parse_argv ~current argv speclist (fun _ -> ()) "...")

let proc_and_eio (name, f) =
  [
    t (name ^ " (non-eio)") f;
    t (name ^ " (eio)") (fun () -> Eio_main.run @@ fun _ -> f ());
  ]

let tests =
  let eio_and_non_tests =
    [
      ("Fiber scope", test_hook_local_with_hook_set_scope);
      ("Fiber nested", test_hook_local_nested);
      ("Fiber and exceptions", test_hook_local_with_exn);
      ("Fiber mutation by CLI parsing", test_cli_unscoped_set);
    ]
    |> List.concat_map proc_and_eio
  in

  let eio_only = [ t "Fiber concurrent (eio)" test_hook_local_concurrent ] in

  Testo.categorize "Hooks" @@ eio_and_non_tests @ eio_only
