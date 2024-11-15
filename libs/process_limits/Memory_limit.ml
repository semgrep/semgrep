(* Martin Jambon
 *
 * Copyright (C) 2021-2023 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Avoid segfaults when the process runs out of memory.

   See also https://gitlab.com/gadmm/memprof-limits/.

  NOTE: Be careful when adding logging, tracing, or other kinds of
  opentelmetry!!! This module uses a GC alarm to do its work, and our telemetry
  library can and will deadlock if it is called from the gc alarm. See the
  comment above the GC alarm function `limit_memory` for more info.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

exception ExceededMemoryLimit of string

(* Why those values? *)
let default_stack_warning_kb = 100
let default_heap_warning_mb = 400

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*
   Fail gracefully if memory becomes insufficient.

   It raises Out_of_memory if we're over the memory limit at the end of a
   major GC cycle.

   See https://discuss.ocaml.org/t/todays-trick-memory-limits-with-gc-alarms/4431
   for detailed explanations.

*)
let run_with_memory_limit _caps ?get_context
    ?(stack_warning_kb = default_stack_warning_kb)
    ?(heap_warning_mb = default_heap_warning_mb) ~mem_limit_mb f =
  if stack_warning_kb < 0 then
    invalid_arg
      (spf "run_with_memory_limit: negative argument stack_warning_kb %i"
         stack_warning_kb);
  if mem_limit_mb < 0 then
    invalid_arg
      (spf "run_with_memory_limit: negative argument mem_limit_mb %i"
         mem_limit_mb);

  let context () =
    match get_context with
    | None -> ""
    | Some get_context ->
        let context_str = get_context () in
        spf "[%s] " context_str
  in
  let mb = 1024 * 1024 in
  let mem_limit = mem_limit_mb * mb in
  let stack_warning = stack_warning_kb * 1024 in
  let stack_already_warned = ref false in
  let heap_warning_start =
    (* If there is a memory limit, and we reach 80% of that limit, then we
     * also warn. Whatever happens first. *)
    let mem_limit_warning = int_of_float (float_of_int mem_limit *. 0.8) in
    if mem_limit_mb =|= 0 || heap_warning_mb < mem_limit_warning then
      heap_warning_mb * mb
    else mem_limit_warning
  in
  let heap_warning = ref heap_warning_start in
  (* NOTE: DO NOT TRACE LIMIT MEMORY!!!
     NOTE: DO NOT LOG ANYTHING HERE WITHOUT APPLYING THE NO TELEMETRY TAG
     SET!!!

     Doing ANY sort of telemetry within a gc alarm can cause deadlocks!!! the no
     telemetry tag set will disable telemetry for logs and so they will be
     reported to the user, just not to any sort of telemetry backend.
  *)
  let limit_memory () =
    let stat = Gc.quick_stat () in
    let heap_bytes = stat.heap_words * (Sys.word_size / 8) in
    let stack_bytes = stat.stack_size * (Sys.word_size / 8) in
    let mem_bytes = heap_bytes + stack_bytes in
    if mem_limit > 0 && mem_bytes > mem_limit then (
      Logs.err (fun m ->
          m ~tags:Tracing.no_telemetry_tag_set
            "%sexceeded heap+stack memory limit: %d bytes (stack=%d, heap=%d)"
            (context ()) mem_bytes stack_bytes heap_bytes);
      raise (ExceededMemoryLimit "Exceeded memory limit"))
    else if !heap_warning > 0 && heap_bytes > !heap_warning then (
      Logs.warn (fun m ->
          m ~tags:Tracing.no_telemetry_tag_set
            "%slarge heap size: %d MiB (memory limit is %d MiB). If a crash \
             follows, you could suspect OOM."
            (context ()) (heap_bytes / mb) mem_limit_mb);
      heap_warning := max (2 * !heap_warning) !heap_warning)
    else if
      stack_warning > 0
      && stack_bytes > stack_warning
      && not !stack_already_warned
    then (
      Logs.warn (fun m ->
          m ~tags:Tracing.no_telemetry_tag_set
            "%slarge stack size: %d bytes. If a crash follows, you should \
             suspect a stack overflow. Make sure the maximum stack size is set \
             to 'unlimited' or to a value greater than %d bytes so as to \
             obtain an exception rather than a segfault."
            (context ()) stack_bytes mem_limit);
      stack_already_warned := true)
  in
  let alarm = Gc.create_alarm limit_memory in
  let res =
    try Common.protect f ~finally:(fun () -> Gc.delete_alarm alarm) with
    | Out_of_memory as exn ->
        (*
         Is it bad to collect a full stack backtrace when we're out of memory?
         Fun.protect does it systematically so we'll assume it's fine.

         See:
         - at the time of writing this:
           https://github.com/ocaml/ocaml/blob/357b42accc160c699219575ab8b952be9594e1d9/stdlib/fun.ml
         - latest: https://github.com/ocaml/ocaml/blob/trunk/stdlib/fun.ml
      *)
        let e = Exception.catch exn in
        (* Try to free up some space. Expensive operation. *)
        Gc.compact ();
        Exception.reraise e
  in
  res
