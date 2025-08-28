(*
   Copyright (c) 2023-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* return whether or not we should show a spinner *)
val should_show_spinner : unit -> bool

(*
  Show a spinner while waiting for the user to sign in.
  delay_ms is the total delay across all frames, in milliseconds.
  We show each frame for 1/100th of the total delay.
*)
val show_spinner : int -> unit
val spinner_async : unit -> 'a Lwt.t
val erase_spinner : unit -> unit
