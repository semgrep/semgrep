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
let t = Testo.create

let test_make_with_state () =
  let cache = SharedMemo.create ~initial_size:257 () in

  let cache_misses = Atomic.make 0 in
  let f =
    SharedMemo.make_with_state cache (fun k ->
        Atomic.incr cache_misses;
        k + 1)
  in

  (* Seed the cache so the reader always has at least one valid key.  The
   * [Domain.spawn] below establishes a happens-before edge, so the seed is
   * visible to both spawned domains. *)
  let largest_written = Atomic.make 0 in
  assert (f 0 = 1);

  let reader =
    Domain.spawn @@ fun () ->
    for _ = 0 to 50000 do
      (* inv: k is on [0..largest_written], so [f k] is expected to be a
       * cache hit.  A miss here would bump [cache_misses] and indicate a
       * lost update. *)
      let lw = Atomic.get largest_written in
      let k = Random.int (1 + lw) in
      assert (k + 1 = f k)
    done
  in
  let writer =
    Domain.spawn @@ fun () ->
    for k = 1 to 50000 do
      assert (k + 1 = f k);
      Atomic.set largest_written k
    done
  in
  Domain.join reader;
  Domain.join writer;
  (* The seed plus the writer's 50000 iterations are the only expected
   * misses.  A reader miss would push this number higher. *)
  Alcotest.(check int) __LOC__ 50001 (Atomic.get cache_misses);
  Alcotest.(check int) __LOC__ 50001 (SharedMemo.length cache)

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
    incr key_fn_calls;
    Int.to_string i
  in
  let f = SharedMemo.make_with_key_fn key_fn (fun i -> i + 1) in
  for _ = 0 to 100 do
    let i = Random.int 1000 in
    Alcotest.(check int) __LOC__ (f i) (i + 1)
  done;
  assert (!key_fn_calls > 0)

let test_remove () =
  let cache = SharedMemo.create () in
  let calls = ref 0 in
  let f =
    SharedMemo.make_with_state cache (fun k ->
        incr calls;
        k + 1)
  in
  assert (f 42 = 43);
  assert (f 42 = 43);
  Alcotest.(check int) __LOC__ 1 !calls;
  SharedMemo.remove cache 42;
  (* After [remove], the next call recomputes. *)
  assert (f 42 = 43);
  Alcotest.(check int) __LOC__ 2 !calls;
  (* [remove] on an absent key is a no-op. *)
  SharedMemo.remove cache 99;
  Alcotest.(check int) __LOC__ 1 (SharedMemo.length cache)

let test_iter () =
  let cache = SharedMemo.create () in
  let f = SharedMemo.make_with_state cache (fun k -> k * 10) in
  List.iter (fun k -> ignore (f k)) [ 1; 2; 3 ];
  let seen = ref [] in
  SharedMemo.iter (fun k v -> seen := (k, v) :: !seen) cache;
  let sorted = List.sort compare !seen in
  Alcotest.(check (list (pair int int)))
    __LOC__
    [ (1, 10); (2, 20); (3, 30) ]
    sorted;
  (* [iter]'s snapshot semantics: it's safe for [f] (here, the memoizer)
   * to be invoked from within the iteration without deadlocking. *)
  let reentrant_hits = ref 0 in
  SharedMemo.iter (fun k _ -> if f k = k * 10 then incr reentrant_hits) cache;
  Alcotest.(check int) __LOC__ 3 !reentrant_hits

let test_should_cache_predicate () =
  (* [?should_cache] lets callers decline to cache specific values. With
     [Result.is_ok] on a result-returning function, [Error] values are returned
     to the caller but not inserted into [cache], so repeated calls re-run [f].
     [Ok] values cache as usual. *)
  let cache = SharedMemo.create () in
  let calls = ref 0 in
  let f k =
    incr calls;
    if k mod 2 = 0 then Ok (k + 1) else Error (Printf.sprintf "odd:%d" k)
  in
  let memo k =
    SharedMemo.make_with_state ~should_cache:Result.is_ok cache f k
  in
  (* Ok values are cached. *)
  Alcotest.(check bool) __LOC__ true (memo 2 = Ok 3);
  Alcotest.(check bool) __LOC__ true (memo 2 = Ok 3);
  Alcotest.(check int) __LOC__ 1 !calls;
  (* Error values are NOT cached: each call re-runs f. *)
  Alcotest.(check bool) __LOC__ true (memo 3 = Error "odd:3");
  Alcotest.(check bool) __LOC__ true (memo 3 = Error "odd:3");
  Alcotest.(check int) __LOC__ 3 !calls

let tests =
  Testo.categorize "SharedMemo"
    [
      t "test_make_with_state" test_make_with_state;
      t "test_make_x_domains" test_make_x_domains;
      t "test_key_fn" test_key_fn;
      t "test_remove" test_remove;
      t "test_iter" test_iter;
      t "test_should_cache_predicate" test_should_cache_predicate;
    ]
