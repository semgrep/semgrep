(*
   Various utilities to deal with git projects.

   Tests are in Unit_semgrepignore.ml
*)

open File.Operators

let is_git_root (path : Fpath.t) : bool =
  match (Unix.stat !!(path / ".git")).st_kind with
  | S_DIR -> true
  | _ -> false
  | exception Unix.Unix_error ((ENOTDIR | ENOENT), _, _) ->
      (* ENOTDIR: path is not a directory
         ENOENT: path/.git doesn't exist *)
      false

let is_git_submodule_root (path : Fpath.t) : bool =
  match (Unix.stat !!(path / ".git")).st_kind with
  | S_REG -> true
  | _ -> false
  | exception Unix.Unix_error ((ENOTDIR | ENOENT), _, _) ->
      (* ENOTDIR: path is not a directory
         ENOENT: path/.git doesn't exist *)
      false

(*
   Split a target path into project root and git path relative to the project
   root.

   start_dir: absolute path from which we start searching for the presence
   of a .git folder.
   start_git_segments: extra path segments relative to start_dir, representing
   the rest of the target path.
*)
let find_git_project_root_abs (start_dir, start_git_segments) =
  let rec loop acc dir =
    if is_git_root dir then
      let ppath = Ppath.create ("" :: acc) in
      match Ppath.normalize_ppath ppath with
      | Ok ppath -> Some (dir, ppath)
      | Error _s -> None
    else
      let name = Fpath.basename dir in
      let parent = Fpath.parent dir |> Fpath.rem_empty_seg in
      (* someone thought the parent of the root should be itself
         rather than an error *)
      if Fpath.equal parent dir then None else loop (name :: acc) parent
  in
  loop start_git_segments start_dir

(* Split path into initial search root and the rest of the path.

   For a relative path a/b: start search in current directory.
   For an absolute path /a/b: start search in containing folder: /a
*)
let initial_root_candidate_of_path path =
  if Fpath.is_rel path then (Fpath.v (Unix.getcwd ()), Fpath.segs path)
  else (Fpath.parent path |> Fpath.rem_empty_seg, [ Fpath.basename path ])

let find_git_project_root path =
  find_git_project_root_abs (initial_root_candidate_of_path path)

let default_project_root = Fpath.v "."

let force_project_root ?(project_root = default_project_root) path =
  match Ppath.in_project ~root:project_root path with
  | Ok git_path -> (project_root, git_path)
  | Error msg -> failwith msg

let find_any_project_root ?fallback_root ?force_root path =
  match force_root with
  | Some (kind, project_root) ->
      let project_root, git_path = force_project_root ~project_root path in
      (kind, project_root, git_path)
  | None -> (
      let start = initial_root_candidate_of_path path in
      match find_git_project_root_abs start with
      | Some (project_root, git_path) ->
          (Project.Git_project, project_root, git_path)
      | None ->
          let project_root, git_path =
            force_project_root ?project_root:fallback_root path
          in
          (Project.Other_project, project_root, git_path))
