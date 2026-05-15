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
   Load and cache .gitignore (or .semgrepignore) files.

   Design: the cache is built eagerly during [create] by walking every
   directory under the project root (skipping [.git/]) and parsing any
   gitignore-like files we find. After [create] returns, the underlying
   [Hashtbl] is sealed as a [ROHashtbl] so that subsequent [find] calls
   are read-only. This lets us share the cache safely across domains in
   parallel target-filtering code.
*)
open Gitignore

type t = {
  project_root : Fpath.t;
  (* List of gitignore-like files to look for in each of the project's
     source folders. *)
  gitignore_filenames : gitignore_filename list;
  (* Sealed cache; built in [create]. *)
  cache : (string, Gitignore_level_index.t option) ROHashtbl.t;
}

let anchor_of_git_path git_path =
  Ppath.segments git_path |> Glob.Pattern.of_path_segments

(* Parse the gitignore-like files in [dir_path] (a Ppath relative to
   project_root) and return the corresponding level, if any. *)
let load_level_for_dir ~project_root ~gitignore_filenames dir_path =
  let anchor = anchor_of_git_path dir_path in
  let path = Ppath.to_fpath ~root:project_root dir_path in
  let patterns =
    List.fold_left
      (fun acc (file : gitignore_filename) ->
        let file_path = Fpath.add_seg path file.filename in
        if Sys_.file_exists (Fpath.to_string file_path) then
          acc
          @ Parse_gitignore.from_file ~format:file.format ~anchor
              ~source_kind:file.source_kind file_path
        else acc)
      [] gitignore_filenames
  in
  match patterns with
  | [] -> None
  | _ :: _ ->
      Some
        (Gitignore_level_index.of_parsed_patterns
           ~level_kind:"in-project gitignore files"
           ~source_name:(Fpath.to_string path) patterns)

let create ?(gitignore_filenames = [ Gitignore.default_gitignore_filename ])
    ~project_root () =
  let mut = Hashtbl.create 256 in
  let root : Fppath.t = { fpath = project_root; ppath = Ppath.root } in
  Fppath.walk_dirs
    ~should_recurse:(fun ppath -> Ppath.last_segment ppath <> ".git")
    root
    (fun dir ->
      let key = Ppath.to_string_fast dir in
      let level =
        try load_level_for_dir ~project_root ~gitignore_filenames dir with
        | e ->
            (* A single malformed [.gitignore] (e.g. a bad regex raising
               [Pcre2.Error]) or a transient I/O failure here must not
               abort the entire cache build. Log + treat as "no gitignore
               at this directory". *)
            (* nosemgrep: no-logs-in-library *)
            Logs.warn (fun m ->
                m
                  "Gitignore_cache: failed to parse gitignore under %s: %s; \
                   treating as if no gitignore present here"
                  (Ppath.to_string_for_tests dir)
                  (Printexc.to_string e));
            None
      in
      Hashtbl.add mut key level);
  { project_root; gitignore_filenames; cache = ROHashtbl.of_hashtbl mut }

let find t dir_path =
  let key = Ppath.to_string_fast dir_path in
  match ROHashtbl.find_opt t.cache key with
  | Some inner -> inner
  | None -> None
