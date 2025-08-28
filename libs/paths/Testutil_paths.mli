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
type file_tree = Dir of string * file_tree list | File of string * file_kind
and file_kind = Regular of string | Symlink of string

val with_file_tree : file_tree -> (Fpath.t -> unit) -> unit
val with_file_trees : file_tree list -> (Fpath.t -> unit) -> unit
