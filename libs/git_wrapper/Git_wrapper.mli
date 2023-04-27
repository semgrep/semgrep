(* Small wrapper around the 'git' command-line program *)

exception Error of string

(* precondition: cwd must be a directory
   This returns a list of paths relative to cwd.
*)
val files_from_git_ls : cwd:Fpath.t -> Fpath.t list
