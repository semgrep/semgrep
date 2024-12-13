(* A project has a kind and a root path. A "project" is usually a code
 * repository, but it can be anything really.
 *)
type t = { kind : kind; root : Rfpath.t } [@@deriving show]

(* The kind of a project, which is usually the kind of version control
   system (VCS) it uses.
*)
and kind =
  (* A git project as determined by calling git. *)
  | Git_project
  (* TODO: these other VCS aren't fully supported (ignored paths and/or
     fast listing of project files like we do with git).
     They're only used to determine the project root. *)
  | Mercurial_project
  | Subversion_project
  | Darcs_project
  (* A gitignore project is a fake git project. It doesn't have a valid
   *  '.git/' folder but its '.gitignore' files should be read as part
   * of the semgrepignore mechanism.
   *)
  | Gitignore_project
  (* A no-VCS project is any project for which we don't support the
     version-control system or for which it is unknown or purposefully
     ignored with a special option.
     A common case includes untarred project tarballs. The user may
     have to resort to the '--project-root' option to specify/force
     the project root where a root '.semgrepignore' may exist.
  *)
  | No_VCS_project
[@@deriving show]

(* The result of searching for the project root from the filesystem path
 * of a scanning root.
 *)
type scanning_root_info = {
  path : Rfpath.t;
  (* Path of a Semgrep scanning root express within the project, relative to
     the project root. *)
  inproject_path : Ppath.t;
}
[@@deriving show]

(* In many CLI tools (e.g., semgrep), the user can specify a set of
 * starting points to start an analysis (e.g., semgrep foo/ bar/).
 * We call those starting points "scanning roots".
 *)
type scanning_roots = {
  project : t;
  (* scanning roots that belong to the project *)
  scanning_roots : scanning_root_info list;
}
[@@deriving show]

(* Convert a scanning root to the type used to represent any file within
   a project *)
val fppath_of_scanning_root_info : scanning_root_info -> Fppath.t

(*
   Provide a similar result as 'find_any_project_root' but don't look
   for a project root. Instead, use the project root provided
   by 'project_root' which defaults to the current directory.
*)
val force_project_root :
  ?project_root:Rfpath.t ->
  Rfpath.t ->
  (Rfpath.t * scanning_root_info, string) Result.t

(*
   Find a project root given a path in this project and determine the project
   kind.

   This returns the project root and the path relative to that root
   (see the scanning_root_info type above).

   If a project root is not found, the project kind is 'Other_project'
   and the project root is what 'fallback_project_root' specifies.

   To keep things simple, the default value of 'fallback_project_root' is
   set to the current folder '.', resolved to a physical path.

   'force_root' can be used to impose the project root rather than guessing
   it. It's useful for testing the gitignore mechanism and stay within
   the confines of the test folder.

   Note that this function does not call any external program like
   'git' or 'hg'; it just explores the file hierarchy to try to detect
   the root of a project.
*)
val find_any_project_root :
  fallback_root:Rfpath.t option ->
  force_novcs:bool ->
  force_root:t option ->
  Fpath.t ->
  (t * scanning_root_info, string) Result.t
