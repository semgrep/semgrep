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
(* Same as print_endline: print the string and a newline, then flush stdout.
 * Just shorter. *)
val pr : string -> unit

(*****************************************************************************)
(* debugging *)
(*****************************************************************************)
(* see also Dumper.ml *)

(* Print a string and a newline to stderr, then flush stderr. The '2'
 * is used to refect that it prints on stderr (file descriptor '2' in Unix). *)
val pr2 : string -> unit

(* Print on stderr any data structure (using Dumper.dump) *)
val pr2_gen : 'a -> unit

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* run by main_boilerplate below at its finalize step before exiting.
 * Can be used for example to display some profiling information
 * (see Profiling.ml as an example)
 *)
val before_exit : (unit -> unit) list ref

(* do some finalize, signal handling, unix exit conversion, etc *)
val main_boilerplate : (unit -> unit) -> unit
