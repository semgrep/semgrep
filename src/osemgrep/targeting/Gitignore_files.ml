(*
   Load and cache .gitignore (or .semgrepignore) files
*)

type t = {
  project_root : Fpath.t;
  gitignore_filenames : string list;
  cache : (string, Gitignore_level.t) Hashtbl.t;
}

let create ?(gitignore_filenames = [ ".gitignore" ]) ~project_root () =
  if Fpath.is_rel project_root then
    invalid_arg
      ("Gitignore_files.create: project root must be an absolute path: "
      ^ Fpath.to_string project_root);
  let cache = Hashtbl.create 100 in
  { project_root; gitignore_filenames; cache }

let anchor_of_git_path git_path =
  Git_path.components git_path |> Glob_matcher.of_path_components

let path_of_git_path root (git_path : Git_path.t) =
  match git_path.components with
  | "" :: xs -> List.fold_left Fpath.add_seg root xs
  | __else__ -> assert false

let load t dir_path =
  let tbl = t.cache in
  let key = Git_path.to_string dir_path in
  match Hashtbl.find_opt tbl key with
  | Some res -> res
  | None ->
      let anchor = anchor_of_git_path dir_path in
      let path = path_of_git_path t.project_root dir_path in
      let patterns =
        List.fold_left
          (fun acc name ->
            let file_path = Fpath.add_seg path name in
            if Sys.file_exists (Fpath.to_string file_path) then
              acc @ Gitignore_syntax.from_file ~anchor file_path
            else acc)
          [] t.gitignore_filenames
      in
      {
        level_kind = "in-project gitignore files";
        source_name = Fpath.to_string path;
        patterns;
      }
