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
  | Other_project
[@@deriving show]

type roots = {
  project : t;
  (* scanning roots that belong to the project *)
  scanning_roots : Fppath.t list;
}

(* TODO? get rid of? seems redundant with all the other type
 * TODO? factorize also with semgrep src/core/Scanning_root.ml
 *)
type scanning_root_info = { project_root : Rfpath.t; inproject_path : Ppath.t }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* when path is not identified as being in a well-defined project *)
let get_project_root_for_nonproject_file (path : Fpath.t) : Rfpath.t =
  if Fpath.is_rel path then Rfpath.getcwd ()
  else Rfpath.of_fpath_exn (Fpath.parent path)

(* alt: use 'git rev-parse --show-toplevel' but this would be git specific
 * and would require to have an external 'git' program.
 *)
let get_project_root_of_fpath_opt (path : Fpath.t) : (kind * Fpath.t) option =
  let candidates =
    [
      (".git", Git_project);
      (".hg", Mercurial_project);
      ("_darcs", Darcs_project);
      (".svn", Subversion_project);
    ]
  in
  let rec aux dir =
    let res =
      candidates
      |> List.find_map (fun (subdir, kind) ->
             let final = !!(dir / subdir) in
             if Sys.file_exists final && Sys.is_directory final then
               Some (kind, dir)
             else None)
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

let find_any_project_root ?fallback_root ?force_root (fpath : Fpath.t) :
    kind * scanning_root_info =
  Log.debug (fun m ->
      m "find_any_project_root: fallback_root=%s force_root=%s %s"
        (Logs_.option Rfpath.show fallback_root)
        (Logs_.option show force_root)
        !!fpath);
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
          (Other_project, force_project_root ?project_root:fallback_root fpath))
