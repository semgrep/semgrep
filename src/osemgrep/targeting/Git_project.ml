(*
   Various utilities to deal with git projects.

   Tests are in Unit_semgrepignore.ml
*)

let is_git_root path =
  match (Unix.stat Fpath.(path / ".git" |> to_string)).st_kind with
  | S_DIR -> true
  | _ -> false
  | exception Unix.Unix_error ((ENOTDIR | ENOENT), _, _) ->
      (* ENOTDIR: path is not a directory
         ENOENT: path/.git doesn't exist *)
      false

(* The argument must be a physical path resolved with 'realpath'. *)
let find_git_project_root_phys phys_path =
  let rec loop acc path =
    if is_git_root path then Some (path, Git_path.create ("" :: acc))
    else
      let name = Fpath.basename path in
      let parent = Fpath.parent path in
      (* someone thought the parent of the root should be itself
         rather than an error *)
      if Fpath.equal parent path then None else loop (name :: acc) parent
  in
  loop [] phys_path

let find_git_project_root path =
  find_git_project_root_phys (Realpath.realpath path)

let default_project_root = Fpath.v "."

let force_project_root ?(project_root = default_project_root) path =
  let project_root = Realpath.realpath project_root in
  match Git_path.in_project ~root:project_root path with
  | Ok git_path -> (project_root, git_path)
  | Error msg -> failwith msg

type kind = Git_project | Other_project

let find_any_project_root ?fallback_root path =
  let path = Realpath.realpath path in
  match find_git_project_root_phys path with
  | Some (project_root, git_path) -> (Git_project, project_root, git_path)
  | None ->
      let project_root, git_path =
        force_project_root ?project_root:fallback_root path
      in
      (Other_project, project_root, git_path)
