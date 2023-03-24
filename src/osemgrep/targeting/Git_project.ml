(*
   Various utilities to deal with git projects.

   Tests are in Unit_semgrepignore.ml
*)

open File.Operators

let is_git_root path =
  match (Unix.stat !!(path / ".git")).st_kind with
  | S_DIR -> true
  | _ -> false
  | exception Unix.Unix_error ((ENOTDIR | ENOENT), _, _) ->
      (* ENOTDIR: path is not a directory
         ENOENT: path/.git doesn't exist *)
      false

(* The argument must be an absolute path. *)
let find_git_project_root_abs abs_dir =
  let rec loop acc path =
    if is_git_root path then Some (path, Git_path.create ("" :: acc))
    else
      let name = Fpath.basename path in
      let parent = Fpath.parent path |> Fpath.rem_empty_seg in
      (* someone thought the parent of the root should be itself
         rather than an error *)
      if Fpath.equal parent path then None else loop (name :: acc) parent
  in
  loop [] abs_dir

let initial_root_candidate_of_path path =
  if Fpath.is_rel path then Fpath.v (Unix.getcwd ())
  else Fpath.parent path |> Fpath.rem_empty_seg

let find_git_project_root path =
  find_git_project_root_abs (initial_root_candidate_of_path path)

let default_project_root = Fpath.v "."

let force_project_root ?(project_root = default_project_root) path =
  match Git_path.in_project ~root:project_root path with
  | Ok git_path -> (project_root, git_path)
  | Error msg -> failwith msg

type kind = Git_project | Other_project [@@deriving show]

let find_any_project_root ?fallback_root ?force_root path =
  match force_root with
  | Some (kind, project_root) ->
      let project_root, git_path = force_project_root ~project_root path in
      (kind, project_root, git_path)
  | None -> (
      let start_root = initial_root_candidate_of_path path in
      match find_git_project_root_abs start_root with
      | Some (project_root, git_path) -> (Git_project, project_root, git_path)
      | None ->
          let project_root, git_path =
            force_project_root ?project_root:fallback_root path
          in
          (Other_project, project_root, git_path))
