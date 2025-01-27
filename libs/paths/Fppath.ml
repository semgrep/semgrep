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
type t = { fpath : Fpath.t; ppath : Ppath.t } [@@deriving show]

let compare a b = Fpath.compare a.fpath b.fpath

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
