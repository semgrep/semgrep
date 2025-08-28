(*
   Copyright (c) 2020-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(**
   Iterate over a file tree.
*)

val fold :
  ?excluded_paths:string list ->
  ?accept_file_name:(string -> bool) ->
  ?accept_dir_name:(string -> bool) ->
  ('acc -> string -> 'acc) ->
  'acc ->
  string list ->
  'acc
(**
   [fold f acc roots] scans the files or directories [roots] and their
   children, applying [f] on each path corresponding to a regular file
   or a link to a regular file. The accumulator [acc] is used to carry
   results along, like the standard [List.fold_left].

   @param excluded_paths is a list of files or directories to not scan.
   @param accept_file_name is an optional filter applied on a
                           file's base name, not its full path. Only applies
                           to regular files or symbolic links to regular files.
   @param accept_dir_name is an optional filter applied on a directory's base
                          name, not its full path. Only applies
                           to directories or symbolic links to directories.
*)

val iter :
  ?excluded_paths:string list ->
  ?accept_file_name:(string -> bool) ->
  ?accept_dir_name:(string -> bool) ->
  (string -> unit) ->
  string list ->
  unit
(** Simplified interface to [fold], operating by side effects. *)

val list :
  ?excluded_paths:string list ->
  ?accept_file_name:(string -> bool) ->
  ?accept_dir_name:(string -> bool) ->
  string list ->
  string list
(** Simplified interface to [fold], which lists all the matching paths. *)
