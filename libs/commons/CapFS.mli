(*
   Copyright (c) 2021-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
val readdir : Unix.dir_handle -> Fpath.t

(* Read the names found in a directory, excluding "." and "..". *)
val read_dir_entries : Fpath.t -> Fpath.t list

(* Note that this calls internally Sys.readdir but does not require
 * the capability because in the end none of the entries are returned
 *)
val is_empty_dir : Fpath.t -> bool

(* [with_chdir dir f] will temporarily change the pwd
 * to [dir] and execute [f] in this context and then restore the pwd to
 * its old value. This internally calls Common.protect so is
 * safe to use even if f throw exceptions
 *)
val with_chdir : Fpath.t -> (unit -> 'a) -> 'a
