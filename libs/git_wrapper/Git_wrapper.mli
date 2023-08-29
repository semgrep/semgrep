(* Small wrapper around the 'git' command-line program *)

exception Error of string

type status = {
  added : string list;
  modified : string list;
  removed : string list;
  unmerged : string list;
  renamed : (string * string) list;
}
[@@deriving show]

(* precondition: cwd must be a directory
   This returns a list of paths relative to cwd.
*)
val files_from_git_ls : cwd:Fpath.t -> Fpath.t list

(* get merge base between arg and HEAD *)
val get_merge_base : string -> string

(* Executing a function inside a directory created from git-worktree.

   `git worktree` is doing 90% of the heavy lifting here. Docs:
   https://git-scm.com/docs/git-worktree

   In short, git allows you to have multiple working trees checked out at
   the same time. This means you can essentially have X different
   branches/commits checked out from the same repo, in different locations

   Different worktrees share the same .git directory, so this is a lot
   faster/cheaper than cloning the repo multiple times

   This also allows us to not worry about git state, since
   unstaged/staged files are not shared between worktrees. This means we
   don't need to git stash anything, or expect a clean working tree.
*)
val run_with_worktree : commit:string -> (unit -> 'a) -> 'a

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
