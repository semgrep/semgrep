(*
   Load and cache .gitignore (or .semgrepignore) files
*)
open Gitignore

let create ?(gitignore_filenames = [ ("gitignore", ".gitignore") ])
    ~project_root () =
  let cache = Hashtbl.create 100 in
  { project_root; gitignore_filenames; cache }

let anchor_of_git_path git_path =
  Ppath.segments git_path |> Glob.Pattern.of_path_segments

let load t dir_path =
  let tbl = t.cache in
  let key = Ppath.to_string dir_path in
  match Hashtbl.find_opt tbl key with
  | Some res -> res
  | None ->
      let anchor = anchor_of_git_path dir_path in
      let path = Ppath.to_fpath t.project_root dir_path in
      let patterns =
        List.fold_left
          (fun acc (kind, name) ->
            let file_path = Fpath.add_seg path name in
            if Sys.file_exists (Fpath.to_string file_path) then
              acc @ Parse_gitignore.from_file ~anchor ~kind file_path
            else acc)
          [] t.gitignore_filenames
      in
      let res =
        match patterns with
        | [] -> None
        | _ :: _ ->
            Some
              ({
                 level_kind = "in-project gitignore files";
                 source_name = Fpath.to_string path;
                 patterns;
               }
                : Gitignore.level)
      in
      Hashtbl.add tbl key res;
      res
  [@@profiling]
