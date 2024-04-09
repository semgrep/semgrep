(* Yoann Padioleau
 *
 * Copyright (C) 2023 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
open Sexplib.Std
open Fpath_.Operators

let tags = Logs_.create_tags [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small wrapper around the 'git' command-line program.
 *
 * alternatives:
 *  - use https://github.com/fxfactorial/ocaml-libgit2, but
 *    pysemgrep uses 'git' directly so easier for now to just imitate.
 *    Morever we need services like 'git ls-files' and this
 *    does not seem to be supported by libgit.
 *  - use https://github.com/mirage/ocaml-git, which implements
 *    git purely in OCaml, but this currently seems to support only
 *    the "core" of git, not all the "porcelain" around such as
 *    'git ls-files' that we need.
 *
 * TODO:
 *  - use Bos uniformly instead of Common.cmd_to_list and Lwt_process.
 *  - be more consistent and requires an Fpath instead
 *    of relying sometimes on cwd.
 *)
(* Standard git uses sha1 *)

module type Store = Git.S with type hash = Digestif.SHA1.t

module Hash = Git.Hash.Make (Digestif.SHA1)
module Value = Git.Value.Make (Hash)
module Commit = Git.Commit.Make (Hash)
module Tree = Git.Tree.Make (Hash)
module Blob = Git.Blob.Make (Hash)
module User = Git.User

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type status = {
  added : string list;
  modified : string list;
  removed : string list;
  unmerged : string list;
  renamed : (string * string) list;
}
[@@deriving show]

(* To make sure we don't call other commands!
 * TODO: we could create a special git_cap capabilities instead of using
 * Cap.exec. We could have
 *   type git_cap
 *   val git_cap_of_exec: Cap.exec -> git_cap
 *)
let git : Cmd.name = Cmd.Name "git"

type hash = Hash.t [@@deriving show, eq, ord]
type value = Value.t [@@deriving show, eq, ord]
type commit = Commit.t [@@deriving show, eq, ord]
type blob = Blob.t [@@deriving show, eq, ord]
type author = User.t [@@deriving show, eq, ord]
type object_table = (hash, value) Hashtbl.t

type blob_with_extra = { blob : blob; path : Fpath.t; size : int }
[@@deriving show]

(*****************************************************************************)
(* Reexports *)
(*****************************************************************************)

let commit_digest = Commit.digest
let commit_author = Commit.author
let hex_of_hash = Hash.to_hex
let blob_digest = Blob.digest
let string_of_blob = Blob.to_string

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

exception Error of string

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* diff unified format regex:
 * https://www.gnu.org/software/diffutils/manual/html_node/Detailed-Unified.html#Detailed-Unified
 * The above documentation isn't great, so unified diff format is
 *
 *    @@ -start,count +end,count @@
 *
 * where count is optional
 * Start and end here are misnomers. Start here refers to where this line
 * starts in the A file being compared.
 * End refers to where this line starts in the B file being compared.
 * So if we have a line that starts at line 10 in the A file, and starts at
 * line 20 in the B file, then we have
 *
 *     @@ -10 +20 @@
 *
 * If we have a multiline diff, then we have
 *
 *     @@ -10,3 +20,3 @@
 *
 * where the 3 is the number of lines that were changed
 * We use a named capture group for the lines, and then split on the comma if
 * it's a multiline diff
 *)
let git_diff_lines_re = Pcre_.regexp {|@@ -\d*,?\d* \+(?P<lines>\d*,?\d*) @@|}
let remote_repo_name_re = Pcre_.regexp {|^http.*\/(.*)\.git$|}
let getcwd () = USys.getcwd () |> Fpath.v

(*
   Create an optional -C option to change directory.

   Usage:

     let cmd = Cmd.(git %% cd cwd % "rev-parse" % "--show-toplevel") in
     ...
*)
let cd (opt_cwd : Fpath.t option) : Cmd.args =
  match opt_cwd with
  | None -> []
  | Some path -> [ "-C"; !!path ]

(* Convert an option to a list. Used to inject optional arguments. *)
let opt (o : string option) : string list =
  match o with
  | None -> []
  | Some str -> [ str ]

(*
   Add an optional flag to the command line by returning a list of arguments
   to add to the current arguments.

   Usage:

   let do_something ?(foo = false) () =
     let cmd = (git, [ "do-something" ] @ flag "--foo" foo) in
     ...
*)
let flag name (is_set : bool) : string list =
  match is_set with
  | true -> [ name ]
  | false -> []

(** Given some git diff ranges (see above), extract the range info *)
let range_of_git_diff lines =
  let range_of_substrings substrings =
    let line = Pcre.get_substring substrings 1 in
    let lines = Str.split (Str.regexp ",") line in
    let first_line =
      match lines with
      | [] -> assert false
      | h :: _ -> h
    in
    let start = int_of_string first_line in
    let change_count =
      if List.length lines > 1 then int_of_string (List.nth lines 1) else 1
    in
    let end_ = change_count + start in
    (start, end_)
  in
  let matched_ranges = Pcre_.exec_all ~rex:git_diff_lines_re lines in
  (* get the first capture group, then optionally split the comma if multiline
     diff *)
  match matched_ranges with
  | Ok ranges ->
      Array.map
        (fun s ->
          try range_of_substrings s with
          | Not_found -> (-1, -1))
        ranges
  | Error _ -> [||]

let remote_repo_name url =
  match Pcre_.exec ~rex:remote_repo_name_re url with
  | Ok (Some substrings) -> Some (Pcre.get_substring substrings 1)
  | _ -> None

let tree_of_commit (objects : object_table) commit =
  commit |> Commit.tree |> Hashtbl.find_opt objects |> fun obj ->
  match obj with
  | Some (Git.Value.Tree tree) -> tree
  | _ ->
      failwith
        "Not a tree! Shouldn't happen, as we read a tree from a commit from a \
         store."

let rec blobs_of_tree ?(path_prefix = "") (objects : object_table)
    (tree : Tree.t) : blob_with_extra list =
  tree |> Tree.to_list |> List.concat_map (blobs_of_entry ~path_prefix objects)

and blobs_of_entry ?(path_prefix = "") (objects : object_table) :
    Tree.entry -> blob_with_extra list = function
  | { perm = `Exec | `Everybody | `Normal; name = path_segment; node = hash } ->
      let blob =
        hash |> Hashtbl.find_opt objects |> fun obj ->
        match obj with
        | Some (Git.Value.Blob blob) -> blob
        | _ ->
            failwith
              "Not a blob! Shouldn't happen, as we read a blob from a tree \
               from a store."
      in
      (* If youre on a 32bit machine trying to scan files with blobs > 2gb you deserve the error this could cause *)
      let size = blob |> Blob.length |> Int64.to_int in
      let path = Filename.concat path_prefix path_segment |> Fpath.v in
      [ { blob; path; size } ]
  | { perm = `Dir; name = path_segment; node = hash } ->
      let tree =
        hash |> Hashtbl.find_opt objects |> fun obj ->
        match obj with
        | Some (Git.Value.Tree tree) -> tree
        | _ ->
            failwith
              "Not a tree! Shouldn't happen, as we read a tree from a tree \
               from a store."
      in
      let path = Filename.concat path_prefix path_segment in
      blobs_of_tree ~path_prefix:path objects tree
  | { perm = `Link; _ }
  | { perm = `Commit; _ } ->
      []

let blobs_by_commit objects commits =
  commits
  |> List_.map (fun commit ->
         let tree = tree_of_commit objects commit in
         (commit, tree))
  |> List_.map (fun (commit, tree) ->
         let blobs = blobs_of_tree objects tree in
         (commit, blobs))

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let commit_blobs_by_date objects =
  Logs.debug (fun m -> m ~tags "getting commits");
  let commits =
    objects |> Hashtbl.to_seq |> List.of_seq
    |> List_.map_filter (fun (_, value) ->
           match value with
           | Git.Value.Commit commit -> Some commit
           | _ -> None)
  in
  Logs.debug (fun m -> m ~tags "got commits");
  Logs.debug (fun m -> m ~tags "sorting commits");
  let commits_by_date =
    commits |> List.sort Commit.compare_by_date |> List.rev
  in
  Logs.debug (fun m -> m ~tags "sorted commits");
  let blobs_by_commit = blobs_by_commit objects commits_by_date in
  Logs.debug (fun m -> m ~tags "got blobs by commit");
  blobs_by_commit

let git_check_output (_caps_exec : < Cap.exec >) (args : Cmd.args) : string =
  let cmd : Cmd.t = (git, args) in
  match UCmd.string_of_run ~trim:true cmd with
  | Ok (str, (_, `Exited 0)) -> str
  | Ok _
  | Error (`Msg _) ->
      Logs.warn (fun m ->
          m
            {|Command failed.
-----
Failed to run %s. Possible reasons:
- the git binary is not available
- the current working directory is not a git repository
- the current working directory is not marked as safe
  (fix with `git config --global --add safe.directory $(pwd)`)

Try running the command yourself to debug the issue.|}
            (Cmd.to_string cmd));
      raise (Error "Error when we run a git command")

type ls_files_kind =
  | Cached (* --cached, the default *)
  | Others (* --others, the complement of Cached but still excluding .git/ *)

let string_of_ls_files_kind (kind : ls_files_kind) =
  match kind with
  | Cached -> "--cached"
  | Others -> "--others"

let ls_files ?(cwd = Fpath.v ".") ?(exclude_standard = false) ?(kinds = [])
    root_paths =
  let roots = root_paths |> List_.map Fpath.to_string in
  let kinds = kinds |> List_.map string_of_ls_files_kind in
  let cmd =
    ( git,
      [ "-C"; !!cwd; "ls-files" ]
      @ kinds
      @ flag "--exclude-standard" exclude_standard
      @ roots )
  in
  let files =
    match UCmd.lines_of_run ~trim:true cmd with
    | Ok (files, (_, `Exited 0)) -> files
    | _ -> raise (Error "Could not get files from git ls-files")
  in
  files |> Fpath_.of_strings

let append_slash_to_dir_path path = Fpath.add_seg path ""

(*
   Make an absolute path relative to a root folder if possible.

   This returns a relative path if possible, otherwise falls back to an
   absolute path. It's possible to obtain a relative path if both paths
   are relative (to the same implicit folder) or if they're both absolute
   and share the same filesystem root ('/' on Unix or a volume name on
   Windows).

   TODO: move to Fpath_?
*)
let relativize_if_possible ~abs_cwd abs_path =
  if not (Fpath.is_abs abs_cwd) then
    invalid_arg
      (spf
         "Git_wrapper.relativize_if_possible: abs_cwd must be an absolute path \
          but we received %s"
         !!abs_cwd);
  if not (Fpath.is_abs abs_path) then
    invalid_arg
      (spf
         "Git_wrapper.relativize_if_possible: abs_path must be an absolute \
          path but we received %s"
         !!abs_path);
  match Fpath.relativize ~root:(append_slash_to_dir_path abs_cwd) abs_path with
  | Some rel_path -> rel_path
  | None -> abs_path

(*
   List files relative to the current directory which may be outside of
   a git project.

   This is something git doesn't allow directly, so we need to perform
   path conversions. The project root must be provided because it's somewhat
   costly to obtain.
*)
let ls_files_relative ?exclude_standard ?kinds ~(project_root : Rpath.t)
    root_paths =
  (* Both project_root and sys_cwd are absolute, physical paths *)
  let project_root = Rpath.to_fpath project_root in
  let sys_cwd = Sys.getcwd () |> Fpath.v in
  let abs_root_paths =
    root_paths
    |> List_.map (fun path ->
           (* git accepts absolute paths to scanning roots but not if they
              contain relative segments
              such as '..':
                OK: /home/user/proj/src
                Rejected: ../proj/src
           *)
           Fpath.(sys_cwd // path |> normalize))
  in
  (* List paths relative to the project root.

     Git returns paths that are relative to 'cwd' which must be within the
     project. The following should work even if 'project_root' is a subfolder
     in the git project but we're not counting on it. *)
  let proj_rel_paths =
    ls_files ~cwd:project_root ?exclude_standard ?kinds abs_root_paths
  in
  let rel_paths =
    proj_rel_paths
    |> List_.map (fun proj_rel_path ->
           relativize_if_possible ~abs_cwd:sys_cwd
             (project_root // proj_rel_path))
  in
  rel_paths

(* TODO: somehow avoid error message on stderr in case this is not a git repo *)
let get_project_root_for_files_in_dir dir =
  let cmd = (git, [ "-C"; !!dir; "rev-parse"; "--show-toplevel" ]) in
  match UCmd.string_of_run ~trim:true cmd with
  | Ok (path, (_, `Exited 0)) -> Some (Fpath.v path)
  | _ -> None

let get_project_root_for_file file =
  get_project_root_for_files_in_dir (Fpath.parent file)

let get_project_root_for_file_or_files_in_dir path =
  if Sys.is_directory !!path then get_project_root_for_files_in_dir path
  else get_project_root_for_file path

let is_tracked_by_git file = get_project_root_for_file file |> Option.is_some

let checkout ?cwd ?git_ref () =
  let cmd = (git, cd cwd @ [ "checkout" ] @ opt git_ref) in
  match UCmd.status_of_run ~quiet:true cmd with
  | Ok (`Exited 0) -> ()
  | _ -> raise (Error "Could not checkout git ref")

(* Why these options?
 * --depth=1: only get the most recent commit
 *  --filter=blob:none: don't get any blobs (file contents) from the repo
 *  --sparse: only get the files we need
 *  --no-checkout: don't checkout the files, we'll do that later
 * See https://github.blog/2020-01-17-bring-your-monorepo-down-to-size-with-sparse-checkout/#sparse-checkout-and-partial-clones
 * for a better explanation
 *
 * The following benchmarks were run scanning the django repo
 * for a bash rule
 * Mini benchmarks:
 * clone then scan repo for : 32.9s
 * depth=1 then scan repo for a bash rule: 4.8s
 * using sparse shallow checkout + sparse checkout adding files
 * determined during the targeting step: 1.09s
 *)
let sparse_shallow_filtered_checkout (url : Uri.t) path =
  let path = Fpath.to_string path in
  let cmd =
    ( git,
      [
        "clone";
        "--depth=1";
        "--filter=blob:none";
        "--sparse";
        "--no-checkout";
        Uri.to_string url;
        path;
      ] )
  in
  match UCmd.status_of_run ~quiet:true cmd with
  | Ok (`Exited 0) -> Ok ()
  | _ -> Error "Could not clone git repo"

(* To be used in combination with [sparse_shallow_filtered_checkout]
   This function will add the files we want to actually pull the blobs for
   and checkout the files we want.
*)
let sparse_checkout_add ?cwd folders =
  let folders = List_.map Fpath.to_string folders in
  let cmd =
    ( git,
      cd cwd
      @ [ "sparse-checkout"; "add"; "--sparse-index"; "--cone" ]
      @ folders )
  in
  match UCmd.status_of_run ~quiet:true cmd with
  | Ok (`Exited 0) -> Ok ()
  | _ -> Error "Could not add sparse checkout"

let get_merge_base commit =
  let cmd = (git, [ "merge-base"; commit; "HEAD" ]) in
  match UCmd.string_of_run ~trim:true cmd with
  | Ok (merge_base, (_, `Exited 0)) -> merge_base
  | _ -> raise (Error "Could not get merge base from git merge-base")

let run_with_worktree (caps : < Cap.chdir ; Cap.tmp >) ~commit ?(branch = None)
    f =
  let cwd = getcwd () |> Fpath.to_dir_path in
  let git_root =
    match get_project_root_for_files_in_dir cwd with
    | None ->
        raise (Error ("Could not get git root for current directory " ^ !!cwd))
    | Some path -> Fpath.to_dir_path path
  in
  let relative_path =
    match Fpath.relativize ~root:git_root cwd with
    | Some p -> p
    | None -> raise (Error "")
  in
  let rand_dir () =
    let uuid = Uuidm.v `V4 in
    let dir_name = "semgrep_git_worktree_" ^ Uuidm.to_string uuid in
    let dir = CapTmp.get_temp_dir_name caps#tmp / dir_name in
    UUnix.mkdir !!dir 0o777;
    dir
  in
  let temp_dir = rand_dir () in
  let cmd : Cmd.t =
    match branch with
    | None -> (git, [ "worktree"; "add"; !!temp_dir; commit ])
    | Some new_branch ->
        (git, [ "worktree"; "add"; !!temp_dir; commit; "-b"; new_branch ])
  in
  match UCmd.status_of_run ~quiet:true cmd with
  | Ok (`Exited 0) ->
      let work () =
        Fpath.append temp_dir relative_path
        |> Fpath.to_string |> CapSys.chdir caps#chdir;
        f ()
      in
      let cleanup () =
        cwd |> Fpath.to_string |> CapSys.chdir caps#chdir;
        let cmd = (git, [ "worktree"; "remove"; !!temp_dir ]) in
        match UCmd.status_of_run ~quiet:true cmd with
        | Ok (`Exited 0) ->
            Logs.info (fun m -> m ~tags "Finished cleaning up git worktree")
        | Ok _ ->
            raise (Error ("Could not remove git worktree at " ^ !!temp_dir))
        | Error (`Msg e) -> raise (Error e)
      in
      Common.protect ~finally:cleanup work
  | Ok _ -> raise (Error ("Could not create git worktree for " ^ commit))
  | Error (`Msg e) -> raise (Error e)

let status ?cwd ?commit () =
  let cmd =
    ( git,
      cd cwd
      @ [
          "diff";
          "--cached";
          "--name-status";
          "--no-ext-diff";
          "-z";
          "--diff-filter=ACDMRTUXB";
          "--ignore-submodules";
          "--relative";
        ]
      @ opt commit )
  in
  let stats =
    match UCmd.string_of_run ~trim:true cmd with
    | Ok (str, (_, `Exited 0)) -> str |> String.split_on_char '\000'
    | _ -> raise (Error "Could not get files from git ls-files")
  in
  let check_dir file =
    try
      match (UUnix.stat file).st_kind with
      | Unix.S_DIR -> true
      | _ -> false
    with
    | _ -> false
  in
  let check_symlink file =
    try
      match (UUnix.lstat file).st_kind with
      | Unix.S_LNK -> true
      | _ -> false
    with
    | _ -> false
  in
  let added = ref [] in
  let modified = ref [] in
  let removed = ref [] in
  let unmerged = ref [] in
  let renamed = ref [] in
  let rec parse = function
    | _ :: file :: tail when check_dir file && check_symlink file ->
        Logs.info (fun m ->
            m ~tags "Skipping %s since it is a symlink to a directory: %s" file
              (UUnix.realpath file));
        parse tail
    | "A" :: file :: tail ->
        added := file :: !added;
        parse tail
    | "M" :: file :: tail ->
        modified := file :: !modified;
        parse tail
    | "D" :: file :: tail ->
        removed := file :: !removed;
        parse tail
    | "U" :: file :: tail ->
        unmerged := file :: !unmerged;
        parse tail
    | "T" (* type changed *) :: file :: tail ->
        if not (check_symlink file) then modified := file :: !modified;
        parse tail
    | typ :: before :: after :: tail when String.starts_with ~prefix:"R" typ ->
        removed := before :: !removed;
        added := after :: !added;
        renamed := (before, after) :: !renamed;
        parse tail
    | "!" (* ignored *) :: _ :: tail -> parse tail
    | "?" (* untracked *) :: _ :: tail -> parse tail
    | unknown :: file :: tail ->
        Logs.warn (fun m ->
            m ~tags "unknown type in git status: %s, %s" unknown file);
        parse tail
    | [ remain ] ->
        Logs.warn (fun m ->
            m ~tags "unknown data after parsing git status: %s" remain)
    | [] -> ()
  in
  parse stats;
  {
    added = !added;
    modified = !modified;
    removed = !removed;
    unmerged = !unmerged;
    renamed = !renamed;
  }

let dirty_lines_of_file ?cwd ?(git_ref = "HEAD") file =
  let cwd =
    match cwd with
    | None -> Some (Fpath.parent file)
    | Some _ -> cwd
  in
  let cmd =
    (* --error-unmatch Returns a non 0 exit code if a file is not tracked by git. This way further on in this function we don't try running git diff on untracked files, as this isn't allowed. *)
    (git, cd cwd @ [ "ls-files"; "--error-unmatch"; !!file ])
  in
  match (UCmd.status_of_run ~quiet:true cmd, git_ref = "HEAD") with
  | _, false
  | Ok (`Exited 0), _ ->
      let cmd = (git, cd cwd @ [ "diff"; "-U0"; git_ref; !!file ]) in
      let lines =
        match UCmd.string_of_run ~trim:true cmd with
        | Ok (lines, (_, `Exited 1))
        (* 1 happens if git doesn't exit cleanly aka file is not in the repo *)
        | Ok (lines, (_, `Exited 0)) ->
            Some lines
        | _ -> None
      in
      Option.bind lines (fun l -> Some (range_of_git_diff l))
  | Ok _, _ -> None
  | Error (`Msg e), _ -> raise (Error e)

let dirty_files ?cwd () =
  let cmd =
    (git, cd cwd @ [ "status"; "--porcelain"; "--ignore-submodules" ])
  in
  let lines =
    match UCmd.lines_of_run ~trim:false cmd with
    | Ok (lines, (_, `Exited 0)) -> lines
    | _ -> []
  in
  (* out_lines splits on newlines, so we always have an extra space at the end *)
  let files = List.filter (fun f -> not (String.trim f = "")) lines in
  let files = List_.map (fun l -> Fpath.v (Str.string_after l 3)) files in
  files

let init ?cwd ?(branch = "main") () =
  let cmd = (git, cd cwd @ [ "init"; "-b"; branch ]) in
  match UCmd.status_of_run cmd with
  | Ok (`Exited 0) -> ()
  | _ -> raise (Error "Error running git init")

let add ?cwd ?(force = false) files =
  let files = List_.map Fpath.to_string files in
  let cmd = (git, cd cwd @ [ "add" ] @ flag "--force" force @ files) in
  match UCmd.status_of_run cmd with
  | Ok (`Exited 0) -> ()
  | _ -> raise (Error "Error running git add")

let commit ?cwd msg =
  let cmd = (git, cd cwd @ [ "commit"; "-m"; msg ]) in
  match UCmd.status_of_run cmd with
  | Ok (`Exited 0) -> ()
  | Ok (`Exited i) ->
      raise (Error (Common.spf "Error running git commit: bad exit %d" i))
  | Ok (`Signaled i) ->
      raise (Error (Common.spf "Error running git commit: bad signal %d" i))
  | Error (`Msg s) ->
      raise (Error (Common.spf "Error running git commit: %s" s))

(* Remove credentials in URL if present (e.g. in GitLab CI) *)
let clean_project_url (url : string) : Uri.t =
  let uri = Uri.of_string url in
  match (Uri.user uri, Uri.password uri) with
  | Some _, Some _ -> Uri.with_userinfo uri None
  | _ -> uri

(* TODO: should return Uri.t option *)
let get_project_url ?cwd () : string option =
  let cmd = (git, cd cwd @ [ "ls-remote"; "--get-url" ]) in
  match UCmd.string_of_run ~trim:true cmd with
  | Ok (url, _) -> Some (Uri.to_string (clean_project_url url))
  | Error _ ->
      UFile.find_first_match_with_whole_line (Fpath.v ".git/config") ".com"

(* TODO(dinosaure): this line is pretty weak due to the [".com"] (what happens
   when the domain is [".io"]?). We probably should handle that by a new
   environment variable. I just copied what [pysemgrep] does.
   [git ls-remote --get-url] is also enough and if we can not get such
   information, that's fine - the metadata is used only [Metrics_] actually. *)

(* coupling: with semgrep_output_v1.atd contribution type *)
let git_log_json_format =
  "--pretty=format:{\"commit_hash\": \"%H\", \"commit_timestamp\": \"%aI\", \
   \"contributor\": {\"commit_author_name\": \"%an\", \"commit_author_email\": \
   \"%ae\"}}"

let time_to_str (timestamp : float) : string =
  let date = Unix.gmtime timestamp in
  let year = date.tm_year + 1900 in
  let month = date.tm_mon + 1 in
  let day = date.tm_mday in
  Printf.sprintf "%04d-%02d-%02d" year month day

(* TODO: should really return a JSON.t list at least *)
let get_git_logs ?cwd ?(since = None) () : string list =
  let cmd : Cmd.t =
    match since with
    | None -> (git, cd cwd @ [ "log"; git_log_json_format ])
    | Some time ->
        let after = spf "--after=\"%s\"" (time_to_str time) in
        (git, cd cwd @ [ "log"; after; git_log_json_format ])
  in
  let lines =
    match UCmd.lines_of_run ~trim:true cmd with
    (* ugly: we should parse those lines and return a proper type,
     * at least a JSON.t.
     *)
    | Ok (lines, (_, `Exited 0)) -> lines
    (* TODO: maybe should raise an Error instead *)
    | _ -> []
  in
  (* out_lines splits on newlines, so we always have an extra space at the end *)
  List.filter (fun f -> not (String.trim f = "")) lines

let cat_file_blob ?cwd (hash : hash) =
  let cmd = (git, cd cwd @ [ "cat-file"; "blob"; Hash.to_hex hash ]) in
  match UCmd.string_of_run ~trim:false cmd with
  | Ok (s, (_, `Exited 0)) -> Ok s
  | Ok (s, _)
  | Error (`Msg s) ->
      Error s

let with_git_repo (files : Testutil_files.t list) func =
  Testutil_files.with_tempfiles_verbose files (fun path ->
      Testutil_files.with_chdir path (fun () ->
          init ();
          add ~force:true [ Fpath.v "." ];
          commit "Add all the files";
          func ()))
