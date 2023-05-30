(*
   Various utilities to deal with git projects.
*)

(*
   Is a given path the root of a git project?
   This works by checking the presence of a folder named '.git/'.
*)
val is_git_root : Fpath.t -> bool

(*
   Is a given path the root of a git submodule?
   This works by checking the presence of a *file* named '.git'.
*)
val is_git_submodule_root : Fpath.t -> bool

(*
   Locate the root folder of the git project starting from
   the given path. The result is the pair
   (path from OS root to git root, path from git root).

   This function is meant to turn a user-specified path into
   paths that are usable with the Semgrepignore module.

   Don't dereference symlinks: the project root of /a/b/c is searched in
   /a/b/ then /a/ then /, regardless of whether they're symlinks or regular
   folders.
   TODO? return an Rpath instead for the git root?
*)
val find_git_project_root : Fpath.t -> (Fpath.t * Ppath.t) option

(*
   Find a project root even if the given path is not within a git project.

   'force_root' can be used to impose the project root rather than guessing
   it. It's useful for testing the gitignore mechanism and stay within
   the confines of the test folder.

   This returns the project root and the path relative to that root just like
   'find_git_project_root'.

   If a git project root is not found, the project kind is 'Other_project'
   and the project root is what 'fallback_project_root' specifies.

   To keep things simple, the default value of 'fallback_project_root' is
   set to the current folder '.', resolved to a physical path.

   TODO? return an Rpath instead for the git root?
*)
val find_any_project_root :
  ?fallback_root:Fpath.t ->
  ?force_root:Project.kind * Fpath.t ->
  Fpath.t ->
  Project.kind * Fpath.t * Ppath.t

(* The default value of '?fallback_project_root' *)
val default_project_root : Fpath.t

(*
   Provide a similar result as 'find_git_project_root' but don't look
   for a git project root. Instead, use the project root provided
   by 'project_root' which defaults to the current directory.
   TODO? take an Rpath instead for the git root?
*)
val force_project_root : ?project_root:Fpath.t -> Fpath.t -> Fpath.t * Ppath.t
