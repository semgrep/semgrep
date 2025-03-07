(* Martin Jambon
 *
 * Copyright (C) 2024-2025 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Fpath_.Operators
module Log = Log_paths.Log

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(*
   List files recursively in a safe, efficient, and portable manner.

   Replaces the functions in libs/commons/ that use external UNIX commands
   such as 'find'.
*)

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

let rec iter_dir_entries caps func dir names =
  List.iter (iter_dir_entry caps func dir) names

and iter_dir_entry caps func (dir : Fpath.t) (name : Fpath.t) =
  iter caps func Fpath.(dir // name)

(*************************************************************************)
(* Entry points *)
(*************************************************************************)

and iter caps func path =
  let stat =
    try Some (Unix.lstat !!path) with
    | Unix.Unix_error (_error_kind, _func, _info) ->
        (* Ignore all errors. Should we ignore less? *)
        None
  in
  match stat with
  | Some { Unix.st_kind = S_DIR; _ } -> iter_dir caps func path
  | Some stat (* regular file, symlink, etc. *) -> func path stat
  | None -> ()

and iter_dir caps func dir =
  let names = CapFS.read_dir_entries caps dir in
  iter_dir_entries caps func dir names

let fold_left caps func init path =
  let acc = ref init in
  iter caps (fun path stat -> acc := func !acc path stat) path;
  !acc

let list_with_stat caps path =
  fold_left caps (fun acc path stat -> (path, stat) :: acc) [] path |> List.rev

let list caps path = list_with_stat caps path |> List_.map fst

(* python: Target.files_from_filesystem *)
let list_regular_files ?(keep_root = false) caps root_path =
  list_with_stat caps root_path
  |> List_.filter_map (fun (path, (stat : Unix.stats)) ->
         Log.debug (fun m -> m "root: %s path: %s" !!root_path !!path);
         if keep_root && path = root_path then Some path
         else
           match stat.st_kind with
           | Unix.S_REG -> Some path
           | _else_ -> None)
