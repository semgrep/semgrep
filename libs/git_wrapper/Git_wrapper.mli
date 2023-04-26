(* Small wrapper around the 'git' command-line program *)

exception Error of string

(* precondition: cwd must be a directory
   This returns a list of paths relative to cwd.
*)
val files_from_git_ls : cwd:Fpath.t -> Fpath.t list

val is_git_repo : unit -> bool Lwt.t
(** Returns true if CWD is a git repo*)

val dirty_lines_of_file : string -> (int * int) array Lwt.t
(** Returns a list of (start, end) line numbers for each dirty line in the file. Assumes that you've checked the file (and CWD) is in a git repo *)

val dirty_files : unit -> string list Lwt.t
(** Returns a list of files that are dirty in the current git repo *)
