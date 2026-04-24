(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Load and cache .gitignore (or .semgrepignore) files
*)
open Gitignore

type t = {
  project_root : Fpath.t;
  (* List of gitignore-like files to look for in each of the project's
     source folders. *)
  gitignore_filenames : gitignore_filename list;
  (* TODO? why we use a cache? Why not loading all those .gitiginore at once?*)
  cache : (string, Gitignore_level_index.t option) Hashtbl.t;
}

let create ?(gitignore_filenames = [ Gitignore.default_gitignore_filename ])
    ~project_root () =
  let cache = Hashtbl.create 100 in
  { project_root; gitignore_filenames; cache }

let anchor_of_git_path git_path =
  Ppath.segments git_path |> Glob.Pattern.of_path_segments

let load t dir_path =
  let tbl = t.cache in
  let key = Ppath.to_string_fast dir_path in
  match Hashtbl.find_opt tbl key with
  | Some res -> res
  | None ->
      let anchor = anchor_of_git_path dir_path in
      let path = Ppath.to_fpath ~root:t.project_root dir_path in
      let patterns =
        List.fold_left
          (fun acc (file : gitignore_filename) ->
            let file_path = Fpath.add_seg path file.filename in
            if Sys_.file_exists (Fpath.to_string file_path) then
              acc
              @ Parse_gitignore.from_file ~format:file.format ~anchor
                  ~source_kind:file.source_kind file_path
            else acc)
          [] t.gitignore_filenames
      in
      let res =
        match patterns with
        | [] -> None
        | _ :: _ ->
            Some
              (Gitignore_level_index.of_parsed_patterns
                 ~level_kind:"in-project gitignore files"
                 ~source_name:(Fpath.to_string path) patterns)
      in
      Hashtbl.add tbl key res;
      res
