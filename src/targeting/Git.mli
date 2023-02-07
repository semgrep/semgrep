(* Small wrapper around the 'git' command-line program *)

exception Error of string

(* precondition: cwd must be a directory *)
val files_from_git_ls : cwd:Common.dirname -> Common.filename list
