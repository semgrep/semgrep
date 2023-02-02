(* Small wrapper around the 'git' command-line program *)

exception Error of string

val files_from_git_ls : cwd:string -> Common.filename list
