(*
   Various utilities to deal with git projects.

   Tests are in Unit_semgrepignore.ml
*)

type scanning_root_info = { project_root : Rfpath.t; inproject_path : Ppath.t }

let force_project_root ?(project_root : Rfpath.t option) (path : Rfpath.t) =
  let project_root =
    match project_root with
    | None -> Rfpath.getcwd ()
    | Some x -> x
  in
  match Ppath.in_project ~root:project_root path with
  | Ok inproject_path -> { project_root; inproject_path }
  | Error msg -> failwith msg

let find_any_project_root ?fallback_root ?force_root (rfpath : Rfpath.t) =
  match force_root with
  | Some (kind, project_root) ->
      let scanning_root_info = force_project_root ~project_root rfpath in
      (kind, scanning_root_info)
  | None -> (
      match Git_wrapper.get_project_root ~cwd:rfpath.fpath () with
      | Some project_root ->
          (* note: this project_root returned by git appears to be
             a physical path already. *)
          let project_root = Rfpath.of_fpath_exn project_root in
          let inproject_path =
            match Ppath.in_project ~root:project_root rfpath with
            | Ok x -> x
            | Error msg -> failwith msg
          in
          (Project.Git_project, { project_root; inproject_path })
      | None ->
          let scanning_root_info =
            force_project_root ?project_root:fallback_root rfpath
          in
          (Project.Other_project, scanning_root_info))
