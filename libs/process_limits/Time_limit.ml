(* Yoann Padioleau, Martin Jambon
 *
 * Copyright (C) 1998-2023 Yoann Padioleau
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
module Log = Log_process_limits.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* A timeout exception with accompanying debug information:
   - a descriptive name
   - the time limit
     The mli interface makes this type private to help prevent unsafe uses of
     the exception. The type is actually defined in the commons compilation
     unit to allow logging to not treat it a an error.
*)
type timeout_info = Exception.timeout_info
type timeout_result_info = Exception.timeout_result_info

exception Timeout = Exception.Timeout

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let string_of_timeout_info { Exception.name; max_duration } =
  spf "%s:%g" name max_duration

(* Only used in non multicore settings *)
(* nosemgrep: no-ref-declarations-at-top-scope *)

(* We use a per domain check since these alarms are per domain if using the gc
   based alarm *)
let current_timer = Domain.DLS.new_key (fun () -> None)
let set_timer timer = Domain.DLS.set current_timer (Some timer)
let clear_timer () = Domain.DLS.set current_timer None

(* Used only in unix/signal based timeouts *)
(* it seems that the toplevel block such signals, even with this explicit
 *  command :(
 *  let _ = Unix.sigprocmask Unix.SIG_UNBLOCK [Sys.sigalrm]
 *)

(* could be in Control section *)

let clear_timer_unix () =
  clear_timer ();
  Unix.setitimer Unix.ITIMER_REAL { Unix.it_value = 0.; it_interval = 0. }
  |> ignore

let set_timer_unix max_duration () info =
  set_timer info;
  Unix.setitimer Unix.ITIMER_REAL
    { Unix.it_value = max_duration; it_interval = 0. }
  |> ignore

let set_timer_gc_based = set_timer
let clear_timer_gc_based = clear_timer

let mk_raise_timeout info start_time =
  let actual_duration = Unix.gettimeofday () -. start_time in
  let result_info = { Exception.actual_duration; exceeded = true } in

  raise (Timeout (info, result_info))

(* [timed_computation_and_clear_timer info max_duration f] is the
   pair [(timed_f, clear_timer)], where

   - [timed_f ()] runs the computation [f], limited by a timelimit that
     is at least of [max_duration]
   - [clear_time ()] will clear the timeout

   The timeout mechanism is selected based on the platform. As of OCaml 5.3,
   support for signals is missing on Windows. Additionally, when using
   multicore, signalsa are delivered to arbitrary domains, making this approach
   unreliable. See gc_alarm_timed_computation_and_clear_timer for how we do
   timeouts when using multicore or on windows
*)
let timed_computation_and_clear_timer info max_duration f :
    (unit -> 'a option * Exception.timeout_result_info) * (unit -> unit) =
  let raise_timeout = mk_raise_timeout info in
  (* We're on a posix compatible system *)
  let clear_timer () = clear_timer_unix () in
  let timed_computation () =
    let start = Unix.gettimeofday () in
    Sys.set_signal Sys.sigalrm
      (Sys.Signal_handle (fun _ -> raise_timeout start));
    set_timer_unix max_duration () info;
    let x = f () in
    clear_timer ();
    let actual_duration = Unix.gettimeofday () -. info.max_duration in
    let result_info = { Exception.actual_duration; exceeded = false } in
    (Some x, result_info)
  in
  (timed_computation, clear_timer)

(* Id of the stack (fiber) the current domain is running on, unique for the
   lifetime of the stack. See Time_limit_stubs.c. *)
external current_stack_id : unit -> int = "semgrep_current_stack_id" [@@noalloc]

(* If we can't use signals we set a gc alarm, since it is checked pretty
   regularly, and we can raise exceptions there.

   As with any asynchronous exception, the alarm raises at whatever point the
   domain happens to be executing, so we must make sure that this point is
   inside the [try ... with Timeout] of [set_timeout]. With eio, this is not a
   given: the guarded computation runs in a fiber that regularly suspends
   into the eio scheduler (e.g. [Lazy_safe.force] performs an effect every
   time), and the domain can also run other fibers in the meantime. A Timeout
   raised there would escape the guard and tear down the scheduler, which
   used to abort whole scans (CODE-9810).

   So the alarm only raises if the domain is currently running on the stack
   that called [set_timeout]. Otherwise it does nothing, and since GC alarms
   run at the end of every major cycle, a later run raises the Timeout once
   the guarded computation is running again. The check is exact because the
   domain can only switch stacks through effect operations, which never
   happen inside the alarm. The downside is that a computation is not
   interrupted while it runs on another stack: while it is suspended, while
   it waits for fibers it forked, or while it runs under an effect handler
   installed inside the guard (we don't install any such handlers ourselves).

   NOTE: we do NOT use the memprof profiler, since memprof profilers are
   exclusive, so if we did do that then we couldn't use it for anything else.
 *)
let gc_alarm_timed_computation_and_clear_timer info max_duration f :
    (unit -> 'a option * Exception.timeout_result_info) * (unit -> unit) =
  let raise_timeout = mk_raise_timeout info in
  let start = Unix.gettimeofday () in
  let started = Atomic.make false in
  (* This is the stack [set_timeout] runs the computation on. *)
  let guarded_stack = current_stack_id () in
  let alarm () =
    let now = Unix.gettimeofday () in
    if
      Atomic.get started
      && Float.compare (now -. start) max_duration > 0
      && Int.equal (current_stack_id ()) guarded_stack
    then raise_timeout start
  in
  let gc_alarm = Gc.create_alarm alarm in
  set_timer_gc_based info;
  let clear_timer () =
    Gc.delete_alarm gc_alarm;
    clear_timer_gc_based ()
  in
  let timed_computation () =
    let f' () =
      Common.protect
        ~finally:(fun () -> Atomic.set started false)
        (fun () ->
          Atomic.set started true;
          f ())
    in
    let x = f' () in
    clear_timer ();
    let actual_duration = Unix.gettimeofday () -. start in
    let result_info = { Exception.actual_duration; exceeded = false } in
    (Some x, result_info)
  in
  (timed_computation, clear_timer)

(*
   This is tricky stuff.

   We have to make sure that timeout is not intercepted before here, so
   avoid exn handle such as try (...) with _ -> cos timeout will not bubble up
   enough. In such case, add a case before such as
   with Timeout -> raise Timeout | _ -> ...

  question: can we have a signal and so exn when in a exn handler ?
*)
let set_timeout ~name ?(sigalrm = false) max_duration f =
  let info = { Exception.name; max_duration } in

  (* Use the old SIGALRM-based timeout mechanism. *)
  (match Domain.DLS.get current_timer with
  | None -> ()
  | Some { Exception.name = running_name; max_duration = running_val } ->
      invalid_arg
        (spf
           "Time_limit.set_timeout: cannot set a timeout %S of %g seconds. A \
            timer for %S of %g seconds is still running."
           name max_duration running_name running_val));
  let timed_f, clear_timer =
    let use_gc_alarm = not sigalrm in
    let res =
      if use_gc_alarm || Sys.win32 then
        gc_alarm_timed_computation_and_clear_timer info max_duration f
      else timed_computation_and_clear_timer info max_duration f
    in
    res
  in
  try
    let res, result_info = timed_f () in
    Process_limit_metrics.record_time_limit ~info ~result_info;
    res
  with
  | Timeout (info, result_info) ->
      Process_limit_metrics.record_time_limit ~info ~result_info;
      clear_timer ();
      Log.warn (fun m -> m "%S timeout at %g s (we abort)" name max_duration);
      None
  | exn ->
      let e = Exception.catch exn in
      (* It's important to disable the alarm before relaunching the exn,
             otherwise the alarm is still running.

             robust?: and if alarm launched after the log (...) ?
             Maybe signals are disabled when process an exception handler ?
          *)
      clear_timer ();
      Log.err (fun m -> m "exn while in set_timeout");
      Exception.reraise e

let set_timeout_opt ~name ?sigalrm time_limit f =
  match time_limit with
  | None -> Some (f ())
  | Some x -> set_timeout ~name ?sigalrm x f
