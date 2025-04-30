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

exception Timeout = Exception.Timeout

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let string_of_timeout_info { Exception.name; max_duration } =
  spf "%s:%g" name max_duration

let current_timer = ref None

(* it seems that the toplevel block such signals, even with this explicit
 *  command :(
 *  let _ = Unix.sigprocmask Unix.SIG_UNBLOCK [Sys.sigalrm]
 *)

(* could be in Control section *)

let clear_timer_unix caps =
  current_timer := None;
  CapUnix.setitimer caps#time_limit Unix.ITIMER_REAL
    { Unix.it_value = 0.; it_interval = 0. }
  |> ignore

let set_timer_unix max_duration caps info =
  current_timer := Some info;
  CapUnix.setitimer caps#time_limit Unix.ITIMER_REAL
    { Unix.it_value = max_duration; it_interval = 0. }
  |> ignore

let clear_timer_win32 () = current_timer := None
let set_timer_win32 info = current_timer := Some info

(* [timed_computation_and_clear_timer info caps max_duration f] is the
   pair [(timed_f, clear_timer)], where

   - [timed_f ()] runs the computation [f], limited by a timelimit that
     is at least of [max_duration]
   - [clear_time ()] will clear the timeout

   The timeout mechanism is selected based on the platform. As of OCaml
   5.3, support for signals is missing on Windows. Use Gc.Memprof
   callbacks to check how much time has elapsed. *)
let timed_computation_and_clear_timer info caps max_duration f :
    (unit -> 'a option) * (unit -> unit) =
  let raise_timeout () = raise (Timeout info) in
  if Sys.win32 then
    let timed_computation () =
      let start = Unix.gettimeofday () in
      let alarm () =
        let now = Unix.gettimeofday () in
        if Float.compare (now -. start) max_duration > 0 then raise_timeout ();
        Some () (* Should we stop tracking the block? *)
      in
      let tracker =
        Gc.Memprof.
          {
            alloc_minor = (fun _alloc -> alarm ());
            alloc_major = (fun _alloc -> alarm ());
            promote = (fun _minor -> alarm ());
            dealloc_minor = (fun _minor -> alarm () |> ignore);
            dealloc_major = (fun _major -> alarm () |> ignore);
          }
      in
      let sampler = Gc.Memprof.start ~sampling_rate:1e-4 tracker in
      set_timer_win32 info;
      let x =
        protect f ~finally:(fun () ->
            Gc.Memprof.(
              stop ();
              discard sampler))
      in
      clear_timer_win32 ();
      Some x
    in
    (timed_computation, clear_timer_win32)
  else
    (* We're on a posix compatible system *)
    let clear_timer () = clear_timer_unix caps in
    let timed_computation () =
      Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise_timeout ()));
      set_timer_unix max_duration caps info;
      let x = f () in
      clear_timer ();
      Some x
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
let set_timeout (caps : < Cap.time_limit >) ~name ~using_eio max_duration f =
  (match !current_timer with
  | None -> ()
  | Some { Exception.name = running_name; max_duration = running_val } ->
      invalid_arg
        (spf
           "Common.set_timeout: cannot set a timeout %S of %g seconds. A timer \
            for %S of %g seconds is still running."
           name max_duration running_name running_val));
  if using_eio then begin
    Log.err (fun m ->
        m "Time limits are disabled with --x-eio, running without time limits");
    Some (f ())
  end
  else
    let info (* private *) = { Exception.name; max_duration } in
    let timed_f, clear_timer =
      timed_computation_and_clear_timer info caps max_duration f
    in
    try timed_f () with
    | Timeout { Exception.name; max_duration } ->
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

let set_timeout_opt ~name ~using_eio time_limit f =
  match time_limit with
  | None -> Some (f ())
  | Some (x, caps) -> set_timeout caps ~name ~using_eio x f
