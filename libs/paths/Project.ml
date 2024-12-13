open Common
open Fpath_.Operators
module Log = Log_paths.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Types and utilities to deal with files in "projects".
 * A project is usually a code repository, but it can be anything really.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = { kind : kind; root : Rfpath.t } [@@deriving show]

and kind =
  | Git_project
  | Mercurial_project
  | Subversion_project
  | Darcs_project
  | Gitignore_project
  | No_VCS_project
[@@deriving show]

type scanning_root_info = {
  path : Rfpath.t;
  (* Path of a Semgrep scanning root express within the project, relative to
     the project root. *)
  inproject_path : Ppath.t;
}
[@@deriving show]

type scanning_roots = {
  project : t;
  (* scanning roots that belong to the project *)
  scanning_roots : scanning_root_info list;
}
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* When the path is not identified as being in a well-defined project,
   we use the following complicated rules:

   - scanning root is a folder: the project root is the folder itself!
   - scanning root is a regular file: the project root is its containing folder

   These rules apply regardless of whether the path is absolute or relative.

   Symbolic links to regular files result in a project root that is
   in general "elsewhere" i.e. not a prefix of the scanning root path:

     scanning root: a.py -> foo/a.py
     project root: /path/to/foo  (not '.')

   To avoid relying on this nonobvious behavior, we recommend that users
   run semgrep on the current folder '.'.

   This function assumes that the path exists.
*)
let get_project_root_for_nonproject_file (path : Rfpath.t) : Rfpath.t =
  let orig_path = path.fpath in
  let phys_path = path |> Rfpath.to_rpath |> Rpath.to_fpath in
  if UFile.is_dir ~follow_symlinks:true phys_path then path
  else if
    (* the original path is a regular file or a symlink to a regular file *)
    (* Be careful with symlinks here! *)
    UFile.is_lnk orig_path
  then
    (* correct but results in an ugly absolute, physical path *)
    phys_path |> Rfpath.of_fpath_exn |> Rfpath.parent
  else
    (* produce a good-looking path but this works only because path
       isn't a symlink *)
    orig_path |> Fpath.parent |> Rfpath.of_fpath_exn

(*
   A git project created with 'git clone' or 'git init' has a '.git/' folder
   but if worktrees are created, their root only has a '.git' file that
   contains a reference to the main worktree that has the '.git/' folder
   with all the data to manage the local repo and its worktrees.

   For example, my current worktree contains this:

     ~/spro2 $ cat .git
     gitdir: /home/martin/spro/.git/worktrees/spro2
*)
let is_git_project_root dir =
  let git_folder_or_file = dir / ".git" in
  if Sys.file_exists !!git_folder_or_file then
    (* TODO: check that the contents of the '.git' look legit? *)
    Some (Git_project, dir)
  else None

(*
   Check for the presence of a special folder at the project root
   such as '.hg'. This is imperfect and could be improved if needed.
*)
let is_project_with_special_dir kind special_dir_name dir =
  let special_dir = !!(dir / special_dir_name) in
  if Sys.file_exists special_dir && Sys.is_directory special_dir then
    Some (kind, dir)
  else None

let is_mercurial_project_root =
  is_project_with_special_dir Mercurial_project ".hg"

let is_darcs_project_root = is_project_with_special_dir Darcs_project "_darcs"

let is_subversion_project_root =
  is_project_with_special_dir Subversion_project ".svn"

(* alt: use 'git rev-parse --show-toplevel' but this would be git specific
 * and would require to have an external 'git' program.
 *)
let get_project_root_of_path_opt (path : Rfpath.t) : (kind * Rfpath.t) option =
  let candidates : (Fpath.t -> (kind * Fpath.t) option) list =
    [
      is_git_project_root;
      is_mercurial_project_root;
      is_darcs_project_root;
      is_subversion_project_root;
    ]
  in
  let rec aux phys_dir =
    let res =
      candidates
      |> List.find_map (fun is_xxx_project_root -> is_xxx_project_root phys_dir)
    in
    match res with
    | Some (project_kind, project_root) ->
        Some (project_kind, Rfpath.of_fpath_exn project_root)
    | None ->
        let parent = Fpath.parent phys_dir in
        (* reached the root of the filesystem *)
        if Fpath.equal parent phys_dir then None else aux parent
  in
  let phys_path = path.rpath |> Rpath.to_fpath in
  let start_dir =
    if UFile.is_dir ~follow_symlinks:false phys_path then phys_path
    else Fpath.parent phys_path
  in
  aux start_dir

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let fppath_of_scanning_root_info (scan_root : scanning_root_info) : Fppath.t =
  { fpath = scan_root.path.fpath; ppath = scan_root.inproject_path }

let check_scanning_root ~project_root:(proj_path : Rfpath.t)
    (scan_path : Rfpath.t) =
  if Rpath.contains proj_path.rpath scan_path.rpath then Ok ()
  else
    Error
      (spf
         "The project root '%s' (real path '%s') does not contain the scanning \
          root '%s' (real path '%s')."
         !!(proj_path.fpath)
         !!(proj_path.rpath |> Rpath.to_fpath)
         !!(scan_path.fpath)
         !!(scan_path.rpath |> Rpath.to_fpath))

let force_project_root ?(project_root : Rfpath.t option) (path : Rfpath.t) :
    (Rfpath.t * scanning_root_info, string) Result.t =
  let project_root =
    match project_root with
    | Some x -> x
    | None -> get_project_root_for_nonproject_file path
  in
  Log.debug (fun m ->
      m "project_root=%s path=%s" (Rfpath.show project_root) (Rfpath.show path));
  match check_scanning_root ~project_root path with
  | Error _ as err -> err
  | Ok () -> (
      match Ppath.in_project ~root:project_root path with
      | Error _ as err -> err
      | Ok inproject_path -> Ok (project_root, { path; inproject_path }))

let find_any_project_root ~fallback_root ~force_novcs ~force_root
    (scanning_root_fpath : Fpath.t) : (t * scanning_root_info, string) Result.t
    =
  Log.debug (fun m ->
      m "find_any_project_root: fallback_root=%s force_root=%s %s"
        (Logs_.option Rfpath.show fallback_root)
        (Logs_.option show force_root)
        !!scanning_root_fpath);
  match Rfpath.of_fpath scanning_root_fpath with
  | Error _ as err -> err
  | Ok path -> (
      let inferred_kind, res =
        match force_root with
        | Some { kind; root = project_root } ->
            (kind, force_project_root ~project_root path)
        | None -> (
            match get_project_root_of_path_opt path with
            | Some (project_kind, project_root) -> (
                match Ppath.in_project ~root:project_root path with
                | Error _ as err -> (project_kind, err)
                | Ok inproject_path ->
                    (project_kind, Ok (project_root, { path; inproject_path })))
            | None ->
                ( No_VCS_project,
                  force_project_root ?project_root:fallback_root path ))
      in
      match res with
      | Error _ as err -> err
      | Ok (project_root, scanning_root_info) ->
          let kind = if force_novcs then No_VCS_project else inferred_kind in
          let project = { kind; root = project_root } in
          Ok (project, scanning_root_info))
