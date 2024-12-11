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

type roots = {
  project : t;
  (* scanning roots that belong to the project *)
  scanning_roots : Fppath.t list;
}
[@@deriving show]

(* TODO? get rid of? seems redundant with all the other type
 * TODO? factorize also with semgrep src/core/Scanning_root.ml
 *)
type scanning_root_info = { project_root : Rfpath.t; inproject_path : Ppath.t }

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
let get_project_root_for_nonproject_file (path : Fpath.t) : Rfpath.t =
  if UFile.is_dir ~follow_symlinks:true path then Rfpath.of_fpath_exn path
  else if
    (* regular file or symlink to a regular file *)
    (* Be careful with symlinks here! *)
    UFile.is_lnk path
  then
    (* correct but results in an ugly absolute, physical path *)
    path |> Rfpath.of_fpath_exn |> Rfpath.parent
  else
    (* produce a good-looking path but this works only because path
       isn't a symlink *)
    path |> Fpath.parent |> Rfpath.of_fpath_exn

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
let get_project_root_of_fpath_opt (path : Fpath.t) : (kind * Fpath.t) option =
  let candidates : (Fpath.t -> (kind * Fpath.t) option) list =
    [
      is_git_project_root;
      is_mercurial_project_root;
      is_darcs_project_root;
      is_subversion_project_root;
    ]
  in
  let rec aux dir =
    let res =
      candidates
      |> List.find_map (fun is_xxx_project_root -> is_xxx_project_root dir)
    in
    match res with
    | Some x -> Some x
    | None ->
        let parent = Fpath.parent dir in
        (* reached the root of the filesystem *)
        if parent = dir then None else aux parent
  in
  if not (Sys.file_exists !!path) then (
    Log.err (fun m ->
        m "get_project_root_of_fpath_opt: not existing path %s" !!path);
    None)
  else
    let rpath = Rpath.of_fpath_exn path in
    let path = Rpath.to_fpath rpath in
    let start_dir =
      if Sys.is_directory !!path then path else Fpath.parent path
    in
    aux start_dir

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let force_project_root ?(project_root : Rfpath.t option) (path : Fpath.t) :
    scanning_root_info =
  let project_root =
    match project_root with
    | Some x -> x
    | None -> get_project_root_for_nonproject_file path
  in
  Log.debug (fun m ->
      m "project_root=%s path=%s" (Rfpath.show project_root) !!path);
  match Ppath.in_project ~root:project_root path with
  | Ok inproject_path -> { project_root; inproject_path }
  | Error msg -> failwith msg

let find_any_project_root ~fallback_root ~force_novcs ~force_root
    (fpath : Fpath.t) : kind * scanning_root_info =
  Log.debug (fun m ->
      m "find_any_project_root: fallback_root=%s force_root=%s %s"
        (Logs_.option Rfpath.show fallback_root)
        (Logs_.option show force_root)
        !!fpath);
  let inferred_kind, root_info =
    match force_root with
    | Some { kind; root = project_root } ->
        (kind, force_project_root ~project_root fpath)
    | None -> (
        match get_project_root_of_fpath_opt fpath with
        | Some (kind, project_root) ->
            let project_root = Rfpath.of_fpath_exn project_root in
            let inproject_path =
              match Ppath.in_project ~root:project_root fpath with
              | Ok x -> x
              | Error msg -> failwith msg
            in
            (kind, { project_root; inproject_path })
        | None ->
            ( No_VCS_project,
              force_project_root ?project_root:fallback_root fpath ))
  in
  let kind = if force_novcs then No_VCS_project else inferred_kind in
  (kind, root_info)
