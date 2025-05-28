let t = Testo.create

let test_make_with_state () =
  let mtx = Mutex.create () in
  let ht = Hashtbl.create 257 in

  let cache_misses = Atomic.make 0 in
  let f =
    SharedMemo.make_with_state mtx ht (fun k ->
        Atomic.incr cache_misses;
        k + 1)
  in

  let largest_written = Atomic.make 0 in
  (* Ensure invariant that keys are on [0..largest_written] *)
  Hashtbl.add ht 0 1;

  let reader =
    Domain.spawn @@ fun () ->
    for _ = 0 to 50000 do
      (* inv: k is on [0..largest_written] *)
      let lw = Atomic.get largest_written in
      let k = Random.int (1 + lw) in

      let v =
        Mutex.protect mtx @@ fun () ->
        match Hashtbl.find_opt ht k with
        | None -> failwith (Printf.sprintf "Lost update? %d %d" lw k)
        | Some n -> n
      in
      assert (k + 1 = v)
    done
  in
  let writer =
    Domain.spawn @@ fun () ->
    for k = 0 to 50000 do
      (* This call to f will be a cache hit. *)
      assert (k + 1 = f k);
      Atomic.set largest_written k
    done
  in
  Domain.join reader;
  Domain.join writer;
  Alcotest.(check int) __LOC__ (1 + Atomic.get cache_misses) (Hashtbl.length ht)

let test_make_x_domains () =
  (* Tests a "realistic" use of a SharedMemo, across fibers schedule
   * by an executor pool. *)
  let f = SharedMemo.make (fun i -> i + 1) in

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let dm = Eio.Stdenv.domain_mgr env in
  let pool = Eio.Executor_pool.create ~sw ~domain_count:4 dm in
  let check () =
    let i = Random.int 1000 in
    assert (f i = i + 1)
  in
  for _ = 1 to 10000 do
    Eio.Executor_pool.submit_exn pool ~weight:1.0 check
  done

let test_key_fn () =
  (* This [f]'s memoizer cache is not int -> int, but string -> int.
   * This test just ensures the key transformation plumbing works.*)
  let key_fn_calls = ref 0 in
  let key_fn =
   fun i ->
    key_fn_calls := !key_fn_calls + 1;
    Int.to_string i
  in
  let f = SharedMemo.make_with_key_fn key_fn (fun i -> i + 1) in
  for _ = 0 to 100 do
    let i = Random.int 1000 in
    Alcotest.(check int) __LOC__ (f i) (i + 1)
  done;
  assert (!key_fn_calls > 0)

let tests =
  Testo.categorize "SharedMemo"
    [
      t "test_make_with_state" test_make_with_state;
      t "test_make_x_domains" test_make_x_domains;
      t "test_key_fn" test_key_fn;
    ]
