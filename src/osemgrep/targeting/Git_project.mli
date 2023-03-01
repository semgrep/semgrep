(*
   Various utilities to deal with git projects.
*)

(*
   Is a given path the root of a git project?
   This works by checking the presence of a folder named '.git'.
*)
val is_git_root : Fpath.t -> bool

(*
   Locate the root folder of the git project starting from
   the given path. The result is the pair
   (physical path from OS root to git root, path from git root).

   Symlinks are dereferenced.

   This function is meant to turn a user-specified path into
   paths that are usable with the Semgrepignore module.
*)
val find_project_root : Fpath.t -> (Fpath.t * Git_path.t) option
