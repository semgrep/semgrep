(*
   Unit tests for out-of-memory errors and stack overflows.

   System's maximum stack size is assumed to be 8 MiB or greater.
   See mli.
*)

open Printf

let t = Testo.create

let get_stack_size_in_bytes () =
  (Gc.quick_stat ()).stack_size * (Sys.word_size / 8)

let get_heap_size_in_bytes () =
  (Gc.quick_stat ()).heap_words * (Sys.word_size / 8)

(*
   This is to show how often the GC alarms run.
*)
let with_debug_alarm f =
  let alarm = Gc.create_alarm (fun () -> printf "Running GC alarm.\n%!") in
  Common.protect f ~finally:(fun () -> Gc.delete_alarm alarm)

(*
   Grow the stack until some limit expressed in bytes.
*)
let grow_stack goal_bytes =
  let rec aux () =
    let stack_size = get_stack_size_in_bytes () in
    if stack_size < goal_bytes then
      (* Allocate enough to trigger GC alarms regularly, before making
         the recursive call. *)
      let data = List.init 100 (fun _ -> ()) in
      (* Prevent tail-call optimization *)
      data :: aux ()
    else (
      (* Trigger the hook that will run the GC alarm. This is cheating. *)
      Gc.full_major ();
      printf "grow_stack: stack reached %i bytes\n%!" stack_size;
      [])
  in
  with_debug_alarm (fun () -> aux () |> ignore)

let grow_heap goal_bytes =
  let rec aux acc =
    let heap_size = get_heap_size_in_bytes () in
    if heap_size < goal_bytes then aux (42 :: acc)
    else (
      printf "grow_heap: heap reached %i bytes\n%!" heap_size;
      (*
         Force-trigger the GC hook. Not great because it's cheating :-|
         The problem is that previous tests may have left a large heap,
         the growing the heap to 'goal_bytes' performs no allocation,
         in which case we don't reach the end of a major cycle.
      *)
      Gc.full_major ())
  in
  aux []

(*
   This test should print a warning.
   TODO: capture the output and check that the warning is there.
*)
let test_stack_warning caps =
  Memory_limit.run_with_memory_limit caps ~stack_warning_kb:100 ~mem_limit_mb:0
    (fun () -> grow_stack 3_000_000)

let test_memory_limit_with_heap caps =
  Gc.full_major ();
  try
    Memory_limit.run_with_memory_limit caps ~mem_limit_mb:10 (fun () ->
        (* Ensure the heap grows to over the limit. *)
        grow_heap 11_000_000);
    assert false
  with
  | Memory_limit.ExceededMemoryLimit _ -> (* success *) ()

let test_memory_limit_with_stack caps =
  try
    Memory_limit.run_with_memory_limit caps ~mem_limit_mb:1 (fun () ->
        grow_stack 1_500_000);
    assert false
  with
  | Memory_limit.ExceededMemoryLimit _ -> (* success *) ()

let skip_if_ocaml_5 tests =
  tests
  |> List_.map (fun test ->
         if Sys.ocaml_release.major = 5 then
           Testo.update ~skipped:(Some "ocaml 5") test
         else test)

let tests (caps : < Cap.memory_limit >) =
  [
    t "stack warning" (fun () -> test_stack_warning caps);
    t "memory limit (heap)" (fun () -> test_memory_limit_with_heap caps);
    t "memory limit (stack)" (fun () -> test_memory_limit_with_stack caps);
  ]
  |> Testo.categorize "memory limits"
  |> skip_if_ocaml_5
