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

let find_project_root path =
  let rec loop acc path =
    let fpath = Realpath.realpath path in
    if is_git_root fpath then Some (fpath, Git_path.create ("" :: acc))
    else
      let name = Fpath.basename fpath in
      let parent = Fpath.parent fpath in
      (* someone thought the parent of the root should be itself
         rather than an error *)
      if Fpath.equal parent fpath then None else loop (name :: acc) parent
  in
  loop [] path
