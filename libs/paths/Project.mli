(* A project has a kind and a root path. A "project" is usually a code
 * repository, but it can be anything really.
 *)
type t = { kind : kind; root : Rfpath.t } [@@deriving show]

(* The kind of a project, which is usually the kind of version control
   system (VCS) it uses.
*)
and kind =
  (* .git/ *)
  | Git_project
  (* .hg/ *)
  | Mercurial_project
  (* .svn/ *)
  | Subversion_project
  (* _darcs/ *)
  | Darcs_project
  (* A gitignore project is a fake git project. It doesn't have a valid
   *  '.git/' folder but its '.gitignore' files should be read as part
   * of the semgrepignore mechanism.
   *)
  | Gitignore_project
  (* whatever else (e.g., untarred project tarballs) where the user will have
   * to use a --project-root option to specify/force the project root.
   *)
  | Other_project
[@@deriving show]

(* In many CLI tools (e.g., semgrep), the user can specify a set of
 * starting points to start an analysis (e.g., semgrep foo/ bar/).
 * We call those starting points "scanning roots" or just "roots".
 *)
type roots = {
  project : t;
  (* scanning roots that belong to the project *)
  scanning_roots : Fppath.t list;
}

(* The result of searching for the project root from the filesystem path
 * of a scanning root.
 *)
type scanning_root_info = {
  (* Physical, absolute path to the project root in the local filesystem
     + original path to be preferred in messages to the user. *)
  project_root : Rfpath.t;
  (* Path of a Semgrep scanning root express within the project, relative to
     the project root. *)
  inproject_path : Ppath.t;
}

(*
   Find a project root given a path in this project.

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
  ?fallback_root:Rfpath.t ->
  ?force_root:t ->
  Fpath.t ->
  kind * scanning_root_info

(*
   Provide a similar result as 'find_any_project_root' but don't look
   for a project root. Instead, use the project root provided
   by 'project_root' which defaults to the current directory.
*)
val force_project_root : ?project_root:Rfpath.t -> Fpath.t -> scanning_root_info
