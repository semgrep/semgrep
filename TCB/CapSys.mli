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
(* Capability-aware wrappers of the dangerous functions in Sys.ml *)

val argv : Cap.Process.argv -> string array
val set_signal : Cap.Process.signal -> int -> Sys.signal_behavior -> unit
val chdir : Cap.Process.chdir -> string -> unit
