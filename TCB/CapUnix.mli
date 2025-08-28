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
(* Capability-aware wrappers of the dangerous functions in Unix.ml *)

(* See also libs/commons/CapExec.ml *)
val execvp : Cap.Exec.t -> string -> string array -> 'a

(* You should use CapExec.ml instead *)
val system : Cap.Exec.t -> string -> Unix.process_status
val fork : Cap.Process.fork -> unit -> int
val alarm : Cap.Process.time_limit -> int -> int

val setitimer :
  Cap.Process.time_limit ->
  Unix.interval_timer ->
  Unix.interval_timer_status ->
  Unix.interval_timer_status
