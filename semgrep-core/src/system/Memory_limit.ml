(*
   Avoid segfaults when the process runs out of memory.
*)

open Common

let logger = Logging.get_logger [ __MODULE__ ]
let default_stack_warning_kb = 100
let default_heap_warning_mb = 400

(*
   Fail gracefully if memory becomes insufficient.

   It raises Out_of_memory if we're over the memory limit at the end of a
   major GC cycle.

   See https://discuss.ocaml.org/t/todays-trick-memory-limits-with-gc-alarms/4431
   for detailed explanations.

*)
let run_with_memory_limit ?(stack_warning_kb = default_stack_warning_kb)
    ?(heap_warning_mb = default_heap_warning_mb) ~mem_limit_mb f =
  if stack_warning_kb < 0 then
    invalid_arg
      (spf "run_with_memory_limit: negative argument stack_warning_kb %i"
         stack_warning_kb);
  if mem_limit_mb < 0 then
    invalid_arg
      (spf "run_with_memory_limit: negative argument mem_limit_mb %i"
         mem_limit_mb);

  let mb = 1024 * 1024 in
  let mem_limit = mem_limit_mb * mb in
  let stack_warning = stack_warning_kb * 1024 in
  let stack_already_warned = ref false in
  let heap_warning_start =
    (* If there is a memory limit, and we reach 80% of that limit, then we
     * also warn. Whatever happens first. *)
    let mem_limit_warning = int_of_float (float_of_int mem_limit *. 0.8) in
    if mem_limit_mb = 0 || heap_warning_mb < mem_limit_warning then
      heap_warning_mb * mb
    else mem_limit_warning
  in
  let heap_warning = ref heap_warning_start in
  let limit_memory () =
    let stat = Gc.quick_stat () in
    let heap_bytes = stat.heap_words * (Sys.word_size / 8) in
    let stack_bytes = stat.stack_size * (Sys.word_size / 8) in
    let mem_bytes = heap_bytes + stack_bytes in
    if mem_limit > 0 && mem_bytes > mem_limit then (
      logger#info
        "exceeded heap+stack memory limit: %d bytes (stack=%d, heap=%d)"
        mem_bytes stack_bytes heap_bytes;
      raise (ExceededMemoryLimit "Exceeded memory limit"))
    else if !heap_warning > 0 && heap_bytes > !heap_warning then (
      logger#warning
        "large heap size: %d MiB (memory limit is %d MiB). If a crash follows, \
         you could suspect OOM."
        (heap_bytes / mb) mem_limit_mb;
      heap_warning := max (2 * !heap_warning) !heap_warning)
    else if
      stack_warning > 0
      && stack_bytes > stack_warning
      && not !stack_already_warned
    then (
      logger#warning
        "large stack size: %d bytes. If a crash follows, you should suspect a \
         stack overflow. Make sure the maximum stack size is set to \
         'unlimited' or to a value greater than %d bytes so as to obtain an \
         exception rather than a segfault."
        stack_bytes mem_limit;
      stack_already_warned := true)
  in
  let alarm = Gc.create_alarm limit_memory in
  try Fun.protect f ~finally:(fun () -> Gc.delete_alarm alarm) with
  | Out_of_memory as e ->
      (* Try to free up some space. Expensive operation. *)
      Gc.compact ();
      raise e
