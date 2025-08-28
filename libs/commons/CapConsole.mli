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
(* Print a string, print a newline, and flush the stdout channel. *)
val print : Cap.Console.stdout -> string -> unit

(* Print a string and flush the stdout channel. *)
val print_no_nl : Cap.Console.stdout -> string -> unit

(* Print a string, print a newline, and flush the stderr channel. *)
val eprint : Cap.Console.stderr -> string -> unit

val ocolor_format_printf :
  Cap.Console.stdout -> ('b, Format.formatter, unit) format -> 'b
