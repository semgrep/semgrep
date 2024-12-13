(* Small wrapper around the 'git' command-line program.
 *
 * The functions in this module will call the external 'git' program, hence
 * the use of the Cap.exec capability.
 * TODO: generalize the use of Cap.exec to all functions!
 *)

(* many of the functions in this module can raise Error if git does not
 * exist or fail to run for some reasons.
 *)
exception Error of string

(* very general helper to run a git command and return its output
 * if everthing went fine or log the error (using Logs) and
 * raise an Error exn otherwise.
 *)
val command : < Cap.exec > -> Cmd.args -> string

type ls_files_kind =
  (* --cached, the default:
   * Show all files cached in Git’s index, i.e. all tracked files
   *)
  | Cached
  (* --others:
   * Show other (i.e. untracked) files in the output,
   * that is mostly the complement of Cached but still
   * excluding .git/
   *)
  | Others

(*
   cwd: directory to cd into (-C)

   The argument is the list of files to start scanning from which defaults
   to the current directory.

   This returns a list of paths relative to cwd.
*)
val ls_files :
  ?cwd:Fpath.t ->
  ?exclude_standard:bool ->
  ?kinds:ls_files_kind list ->
  Fpath.t list ->
  Fpath.t list

(* get merge base between arg and HEAD *)
val merge_base : string -> string

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
val run_with_worktree :
  < Cap.chdir ; Cap.tmp > ->
  commit:string ->
  ?branch:string ->
  (unit -> 'a) ->
  'a

type status = {
  added : string list;
  modified : string list;
  removed : string list;
  unmerged : string list;
  renamed : (string * string) list;
}
[@@deriving show]

(* git status *)
val status : ?cwd:Fpath.t -> ?commit:string -> unit -> status

(*
   Find the root of the git worktree for any files contained in the
   specified folder.

   For example, "/projects/my-git-project" will return
   (Some "/projects/my-git-project")
   if "/projects/my-git-project" is the root of a git repo (or worktree).

   'None' is returned in case of an error.
*)
val project_root_for_files_in_dir : Fpath.t -> Fpath.t option

(*
   Determine the project root for a *member* of a git project.

   If the argument is the root of git submodule, the root of the parent
   project is returned.

   For example, "/projects/my-git-project" will return None
   even though "/projects/my-git-project" is the root of a git repo.
*)
val project_root_for_file : Fpath.t -> Fpath.t option

(*
   If the argument is a directory, return the project root associated with
   the files it contains. If the argument is another kind of file
   (normally a regular file or a symlink), then the root of the project
   containing this file is returned.
*)
val project_root_for_file_or_files_in_dir : Fpath.t -> Fpath.t option

(* Determine whether a path is tracked by git. *)
val is_tracked_by_git : Fpath.t -> bool

val checkout : ?cwd:Fpath.t -> ?git_ref:string -> unit -> unit
(** Checkout the given optional ref *)

val sparse_shallow_filtered_checkout : Uri.t -> Fpath.t -> (unit, string) result
(** Checkout the given commit in the given directory, but only
    the files that are tracked by git and that are not in the
    sparse-checkout config.
    This is useful to avoid checking out the whole repo when
    we only need a few files. *)

val sparse_checkout_add : ?cwd:Fpath.t -> Fpath.t list -> (unit, string) result
(** Add the given files to the sparse-checkout config *)

(* precondition: cwd must be a directory *)
val dirty_lines_of_file :
  ?cwd:Fpath.t -> ?git_ref:string -> Fpath.t -> (int * int) array option
(** [dirty_lines_of_file path] will return an optional array of line ranges that indicate what
  * lines have been changed. An optional [git_ref] can be passed that will be used
  * to diff against. The default [git_ref] is ["HEAD"]
  *)

(* precondition: cwd must be a directory *)
val dirty_paths : ?cwd:Fpath.t -> unit -> Fpath.t list
(** [dirty_paths ()] is the list of paths which are dirty in a git repo, i.e.,
    paths which differ at all from the current index to the HEAD commit, plus
    untracked files. Note that this means this list includes paths which were
    deleted.
    We use "paths" instead of "files" here because it may include directories,
    for newly created directories!
  *)

val init : ?cwd:Fpath.t -> ?branch:string -> unit -> unit
(** [init ()] creates an empty git repository in the current directory. If
    [cwd] is specified, its value is passed to git's [-C] flag. If
    [branch] is specified, it is used as the name of the default branch.
    Otherwise the default branch is named 'main' to avoid warnings that depend
    on the git version.

    Initialize a git repo in the given directory.
    The branch is set by default to 'main' to avoid warnings that depend
    on the git version.
*)

(* Set or replace an entry in the user's config tied to the repo. *)
val config_set : ?cwd:Fpath.t -> string -> string -> unit

(* Get the value of an entry in the user's config. *)
val config_get : ?cwd:Fpath.t -> string -> string option

val gc : ?cwd:Fpath.t -> unit -> unit
(** [gc ()] executes [git gc] in the current directory. If [cwd] is specified,
    its value is passed to git's [-C] flag. *)

val add : ?cwd:Fpath.t -> ?force:bool -> Fpath.t list -> unit
(** [add files] adds the [files] to the git index. *)

val commit : ?cwd:Fpath.t -> string -> unit
(** [commit msg] creates a commit with containing the current contents of the
    index with [msg] as the commit message. *)

val project_url : ?cwd:Fpath.t -> unit -> string option
(** [project_url ()] tries to get the URL of the project from
    [git ls-remote] or from the [.git/config] file. It returns [None] if it
    found nothing relevant.
    TODO: should maybe raise an exn instead if not run from a git repo.
*)

type contribution = {
  commit_hash : string;
  (* datetime *)
  commit_timestamp : string;
  commit_author_name : string;
  commit_author_email : string;
}

val logs : ?cwd:Fpath.t -> ?since:float -> < Cap.exec > -> contribution list
(** [logs ()] will run 'git log' in the current directory and returns for each
    log a contribution record.
    It returns an empty list if it found nothing relevant.
    You can use the [since] parameter to restrict the logs to the commits since
    the specified time.
 *)

type hash = Digestif.SHA1.t [@@deriving show, eq, ord]
type value = hash Git.Value.t [@@deriving show, eq, ord]
type commit = hash Git.Commit.t [@@deriving show, eq, ord]
type author = Git.User.t [@@deriving show, eq, ord]
type blob = Git.Blob.t [@@deriving show, eq, ord]
type object_table = (hash, value) Hashtbl.t

type blob_with_extra = { blob : blob; path : Fpath.t; size : int }
[@@deriving show]

val commit_digest : commit -> hash
(** [commit_digest commit] is the SHA of the commit*)

val commit_author : commit -> author
(** [commit_author commit] is the author of the commit*)

val blob_digest : blob -> hash
(** [blob_digest blob] is the SHA of the blob*)

val string_of_blob : blob -> string
(** [string_of_blob blob] is the content of the blob*)

val hex_of_hash : hash -> string
(** [hex_of_hash hash] is the hexadecimal representation of the hash*)

val commit_blobs_by_date : object_table -> (commit * blob_with_extra list) list
(** [commit_blobs_by_date store] is the list of commits and the blobs they reference, ordered by date, newest first*)

val cat_file_blob : ?cwd:Fpath.t -> hash -> (string, string) result
(** [cat_file_blob sha] will run [git cat-file blob sha] and return either
    {ul
      {- [Ok contents], where [contents] is the contents of the blob; or}
      {- [Error message] where [message] is a brief message indicating why git
      could not perform the action, e.g., [hash] is not the sha of a blob or
      [hash] does not designate an object.}
    }
 *)

val remote_repo_name : string -> string option
(** [remote_repo_name "https://github.com/semgrep/semgrep.git"] will return [Some "semgrep"] *)
