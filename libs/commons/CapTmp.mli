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
(* Capability aware wrappers to UTmp.ml *)

val with_temp_file :
  ?contents:string ->
  ?persist:bool ->
  ?prefix:string ->
  ?suffix:string ->
  ?temp_dir:Fpath.t ->
  Cap.FS.tmp ->
  (Fpath.t -> 'a) ->
  'a

val temp_dir :
  Cap.FS.tmp -> ?temp_dir:Fpath.t -> ?perms:int -> string -> string -> Fpath.t

val get_temp_dir_name : Cap.FS.tmp -> Fpath.t

val new_temp_file :
  ?prefix:string -> ?suffix:string -> ?temp_dir:Fpath.t -> Cap.FS.tmp -> Fpath.t

val replace_named_pipe_by_regular_file_if_needed :
  Cap.FS.tmp -> ?prefix:string -> Fpath.t -> Fpath.t option

val replace_stdin_by_regular_file :
  Cap.FS.tmp -> ?prefix:string -> unit -> Fpath.t

val erase_temp_files : Cap.FS.tmp -> unit
