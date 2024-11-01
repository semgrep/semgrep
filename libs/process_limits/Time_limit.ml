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

(*
   This is tricky stuff.

   We have to make sure that timeout is not intercepted before here, so
   avoid exn handle such as try (...) with _ -> cos timeout will not bubble up
   enough. In such case, add a case before such as
   with Timeout -> raise Timeout | _ -> ...

  question: can we have a signal and so exn when in a exn handler ?
*)
let set_timeout (caps : < Cap.time_limit >) ~name max_duration f =
  (match !current_timer with
  | None -> ()
  | Some { Exception.name = running_name; max_duration = running_val } ->
      invalid_arg
        (spf
           "Common.set_timeout: cannot set a timeout %S of %g seconds. A timer \
            for %S of %g seconds is still running."
           name max_duration running_name running_val));
  let info (* private *) = { Exception.name; max_duration } in
  let raise_timeout () = raise (Timeout info) in
  let clear_timer () =
    current_timer := None;
    CapUnix.setitimer caps#time_limit Unix.ITIMER_REAL
      { Unix.it_value = 0.; it_interval = 0. }
    |> ignore
  in
  let set_timer () =
    current_timer := Some info;
    CapUnix.setitimer caps#time_limit Unix.ITIMER_REAL
      { Unix.it_value = max_duration; it_interval = 0. }
    |> ignore
  in
  try
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise_timeout ()));
    set_timer ();
    let x = f () in
    clear_timer ();
    Some x
  with
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

let set_timeout_opt ~name time_limit f =
  match time_limit with
  | None -> Some (f ())
  | Some (x, caps) -> set_timeout caps ~name x f
