(*
   Avoid segfaults when the process runs out of memory.
*)

open Common

let logger = Logging.get_logger [ __MODULE__ ]

let default_stack_warning_kb = 100

(*
   Fail gracefully if memory becomes insufficient.

   It raises Out_of_memory if we're over the memory limit at the end of a
   major GC cycle.

   See https://discuss.ocaml.org/t/todays-trick-memory-limits-with-gc-alarms/4431
   for detailed explanations.
*)
let run_with_memory_limit ?(stack_warning_kb = default_stack_warning_kb)
    ~mem_limit_mb f =
  if stack_warning_kb < 0 then
    invalid_arg
      (spf "run_with_memory_limit: negative argument stack_warning_kb %i"
         stack_warning_kb);
  if mem_limit_mb < 0 then
    invalid_arg
      (spf "run_with_memory_limit: negative argument mem_limit_mb %i"
         mem_limit_mb);

  let mem_limit = mem_limit_mb * 1024 * 1024 in
  let stack_warning = stack_warning_kb * 1024 in
  let stack_already_warned = ref false in
  let limit_memory () =
    let stat = Gc.quick_stat () in
    let heap_bytes = stat.heap_words * (Sys.word_size / 8) in
    let stack_bytes = stat.stack_size * (Sys.word_size / 8) in
    let mem_bytes = heap_bytes + stack_bytes in
    if mem_limit > 0 && mem_bytes > mem_limit then (
      logger#info
        "exceeded heap+stack memory limit: %d bytes (stack=%d, heap=%d)"
        mem_bytes stack_bytes heap_bytes;
      raise Out_of_memory)
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
  try Fun.protect f ~finally:(fun () -> Gc.delete_alarm alarm)
  with Out_of_memory as e ->
    (* Try to free up some space. Expensive operation. *)
    Gc.compact ();
    raise e
