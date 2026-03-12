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
val pysemgrep : string array -> Exit_code.t
(** Dispatch to pysemgrep, returning its exit code. On Unix this replaces the
    current process via [execvp] and never returns; on Windows it spawns a
    subprocess and waits for it. *)

(* To be used to signal we want to fallback to pysemgrep. The exception
 * must still be handled in the caller which then must call explicitely
 * pysemgrep().
 *)
exception Fallback
