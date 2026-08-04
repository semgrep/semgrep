(* Martin Jambon, Yoann Padioleau
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

(*
   Path information for a file in a project, which includes:
   - a path in the file system
   - the path relative to the project root
*)

(* For gitignore filtering, we need to operate on Ppath (see
 * the signature of Gitignore_filter.select()), but when semgrep
 * displays findings or errors, we want filenames derived from
 * the scanning roots, not the root of the project. This is why we need to
 * keep both the fpath and ppath for each target file as we walked
 * down the filesystem hierarchy.
 *)
type t = { fpath : Fpath.t; ppath : Ppath.t } [@@deriving show, eq]

let to_fpath x = x.fpath
let compare a b = Fpath.compare a.fpath b.fpath

let add_seg t name =
  { fpath = Fpath.(t.fpath / name); ppath = Ppath.add_seg t.ppath name }

let rec walk_dirs ?(should_recurse = fun _ -> true) (dir : t) f =
  f dir.ppath;
  match CapFS.read_dir_entries dir.fpath with
  | exception e ->
      (* nosemgrep: no-logs-in-library *)
      Logs.warn (fun m ->
          m "Fppath.walk_dirs: read_dir_entries on %a failed: %s" Fpath.pp
            dir.fpath (Printexc.to_string e))
  | entries ->
      List.iter
        (fun entry ->
          let child = add_seg dir (Fpath.basename entry) in
          if not (should_recurse child.ppath) then ()
          else
            match UUnix.lstat child.fpath with
            | Ok { st_kind = Unix.S_DIR; _ } ->
                walk_dirs ~should_recurse child f
            | Ok _ ->
                (* Non-directory entry — nothing to recurse into. *)
                ()
            | Error (code, _fun, info) ->
                (* nosemgrep: no-logs-in-library *)
                Logs.warn (fun m ->
                    m "Fppath.walk_dirs: lstat %a: %s %s" Fpath.pp child.fpath
                      (Unix.error_message code) info))
        entries

let append_relative_fpath root fpath =
  let fpath_append a b =
    match Fpath.to_string a with
    | "."
    | "./" ->
        b
    | _ -> Fpath.append a b
  in
  {
    fpath = fpath_append root.fpath fpath;
    ppath = Ppath.append_fpath root.ppath fpath;
  }

let of_relative_fpath_exn fpath =
  { fpath; ppath = Ppath.of_relative_fpath_exn fpath }

let of_file_basename fpath =
  let ppath =
    match Fpath.basename fpath with
    | "" (* / *) -> Ppath.root
    | name -> Ppath.add_seg Ppath.root name
  in
  { fpath; ppath }

(* Use the fpath as ppath, which sort of works for some filtering purposes
   in tests. It's best to not use it. *)
let fake_from_fpath_DEPRECATED fpath =
  { fpath; ppath = Ppath.fake_from_fpath_DEPRECATED fpath }

let unfilterable_ppath = Ppath.create [ ""; "__UNFILTERABLE_TARGET__" ]
let unfilterable_DEPRECATED fpath = { fpath; ppath = unfilterable_ppath }
let is_filterable_DEPRECATED x = x.ppath <> unfilterable_ppath
