(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Contains the name given by the user to the timer and the time limit *)
type timeout_info
type timeout_result_info

(*
   If ever caught, this exception must be re-raised immediately so as
   to not interfere with the timeout handler. See function 'set_timeout'.
*)
exception Timeout of (timeout_info * timeout_result_info)

(* Show name and time limit in a compact format for debugging purposes. *)
val string_of_timeout_info : timeout_info -> string

(*
   Launch the specified computation and abort if it takes longer than
   specified (in seconds).  If [sigalrm] is set, revert to the legacy
   signal-based timeout mechanism; otherwise, use the GC alarm-based one.

   This uses a global timer. An Invalid_argument exception will be raised
   if the timer is already running.

   tl;dr nesting will fail
*)
val set_timeout :
  name:string -> ?sigalrm:bool -> float -> (unit -> 'a) -> 'a option

(*
   Only set a timer if a time limit is specified. Uses 'set_timeout'.
*)
val set_timeout_opt :
  name:string -> ?sigalrm:bool -> float option -> (unit -> 'a) -> 'a option
