(* Small wrapper around the 'git' command-line program *)

exception Error of string

type status [@@deriving show]

(* precondition: cwd must be a directory
   This returns a list of paths relative to cwd.
*)
val files_from_git_ls : cwd:Fpath.t -> Fpath.t list

(* git status *)
val status : cwd:Fpath.t -> commit:string -> status

(* precondition: cwd must be a directory *)
val is_git_repo : Fpath.t -> bool
(** Returns true if passed directory a git repo*)

(* precondition: cwd must be a directory *)
val dirty_lines_of_file : Fpath.t -> (int * int) array option
(** Returns a list of (start, end) line numbers for each dirty line in the file, or none if file is untracked. Assumes that you've checked the file is in a git repo *)

(* precondition: cwd must be a directory *)
val dirty_files : Fpath.t -> Fpath.t list
(** Returns a list of files that are dirty in a git repo *)

val init : Fpath.t -> unit
(** Initialize a git repo in the given directory *)

val add : Fpath.t -> Fpath.t list -> unit
(** Add the given files to the git repo *)

val commit : Fpath.t -> string -> unit
(** Commit the given files to the git repo with the given message *)

val get_project_url : unit -> string option
(** [get_project_url ()] tries to get the URL of the project from
    [git ls-remote] or from the [.git/config] file. It returns [None] if it
    found nothing relevant. *)
