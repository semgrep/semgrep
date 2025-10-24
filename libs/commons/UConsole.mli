(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* See Console.mli for more information
 *
 * DO NOT USE THIS FILE! You should use CapConsole.mli instead.
 *)

(*
   Set the global state indicating whether we want text to use color and
   font highlighting. The default is 'Auto'. The original state is restored
   upon termination.

   Auto: if stdout or stderr is not a terminal, highlighting is turned off.
*)
val with_setup : Console.highlight_setting -> (unit -> 'a) -> 'a

(* Print a string, print a newline, and flush the stdout channel. *)
val print : string -> unit

(* Print a string and flush the stdout channel. *)
val print_no_nl : string -> unit

(* Print a string, print a newline, and flush the stderr channel.
 * You should avoid using this function; Prefer Logs.err or Logs.warn
 * in general.
 *)
val eprint : string -> unit
