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
open Fpath_.Operators

let logger = Logging.get_logger [ __MODULE__ ]

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

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* TODO: could also do
 * [type git_cap] abstract type and then
 * let git_cap_of_exec _caps = unit
 *)

type status = {
  added : string list;
  modified : string list;
  removed : string list;
  unmerged : string list;
  renamed : (string * string) list;
}
[@@deriving show]

let git : Cmd.name = Cmd.Name "git"

type obj_type = Tag | Commit | Tree | Blob [@@deriving show]
type sha = string [@@deriving show]

(* See <https://git-scm.com/book/en/v2/Git-Internals-Git-Objects> *)
type 'extra obj = { kind : obj_type; sha : sha; extra : 'extra }
[@@deriving show]

type batch_check_extra = { size : int } [@@deriving show]
type ls_tree_extra = { path : Fpath.t } [@@deriving show]

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
let _git_diff_lines_re = {|@@ -\d*,?\d* \+(?P<lines>\d*,?\d*) @@|}
let git_diff_lines_re = Pcre_.regexp _git_diff_lines_re
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

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let git_check_output _caps (args : Cmd.args) : string =
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

let ls_files ?(cwd = Fpath.v ".") ?(kinds = []) root_paths =
  let roots = root_paths |> List_.map Fpath.to_string in
  let kinds = kinds |> List_.map string_of_ls_files_kind in
  let cmd = (git, [ "-C"; !!cwd; "ls-files" ] @ kinds @ roots) in
  Logs.info (fun m -> m "Running external command: %s" (Cmd.to_string cmd));
  let files =
    match UCmd.lines_of_run ~trim:true cmd with
    | Ok (files, (_, `Exited 0)) -> files
    | _ -> raise (Error "Could not get files from git ls-files")
  in
  files |> Fpath_.of_strings

let get_project_root ?cwd () =
  let cmd = (git, cd cwd @ [ "rev-parse"; "--show-toplevel" ]) in
  match UCmd.string_of_run ~trim:true cmd with
  | Ok (path, (_, `Exited 0)) -> Some (Fpath.v path)
  | _ -> None

let get_superproject_root ?cwd () =
  let cmd =
    (git, cd cwd @ [ "rev-parse"; "--show-superproject-working-tree" ])
  in
  match UCmd.string_of_run ~trim:true cmd with
  | Ok ("", (_, `Exited 0)) -> get_project_root ?cwd ()
  | Ok (path, (_, `Exited 0)) -> Some (Fpath.v path)
  | _ -> None

let get_project_root_exn () =
  match get_project_root () with
  | Some path -> path
  | _ -> raise (Error "Could not get git root from git rev-parse")

let get_merge_base commit =
  let cmd = (git, [ "merge-base"; commit; "HEAD" ]) in
  match UCmd.string_of_run ~trim:true cmd with
  | Ok (merge_base, (_, `Exited 0)) -> merge_base
  | _ -> raise (Error "Could not get merge base from git merge-base")

let run_with_worktree ~commit ?(branch = None) f =
  let cwd = getcwd () |> Fpath.to_dir_path in
  let git_root = get_project_root_exn () |> Fpath.to_dir_path in
  let relative_path =
    match Fpath.relativize ~root:git_root cwd with
    | Some p -> p
    | None -> raise (Error "")
  in
  let rand_dir () =
    let uuid = Uuidm.v `V4 in
    let dir_name = "semgrep_git_worktree_" ^ Uuidm.to_string uuid in
    let dir = Filename.concat (UFilename.get_temp_dir_name ()) dir_name in
    UUnix.mkdir dir 0o777;
    dir
  in
  let temp_dir = rand_dir () in
  let cmd : Cmd.t =
    match branch with
    | None -> (git, [ "worktree"; "add"; temp_dir; commit ])
    | Some new_branch ->
        (git, [ "worktree"; "add"; temp_dir; commit; "-b"; new_branch ])
  in
  match UCmd.status_of_run ~quiet:true cmd with
  | Ok (`Exited 0) ->
      let work () =
        Fpath.append (Fpath.v temp_dir) relative_path
        |> Fpath.to_string |> UUnix.chdir;
        f ()
      in
      let cleanup () =
        cwd |> Fpath.to_string |> UUnix.chdir;
        let cmd = (git, [ "worktree"; "remove"; temp_dir ]) in
        match UCmd.status_of_run ~quiet:true cmd with
        | Ok (`Exited 0) -> logger#info "Finished cleaning up git worktree"
        | Ok _ -> raise (Error ("Could not remove git worktree at " ^ temp_dir))
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
        logger#info "Skipping %s since it is a symlink to a directory: %s" file
          (UUnix.realpath file);
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
        logger#warning "unknown type in git status: %s, %s" unknown file;
        parse tail
    | [ remain ] ->
        logger#warning "unknown data after parsing git status: %s" remain
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

let is_git_repo ?cwd () =
  let cmd = (git, cd cwd @ [ "rev-parse"; "--is-inside-work-tree" ]) in
  match UCmd.status_of_run ~quiet:true cmd with
  | Ok (`Exited 0) -> true
  | Ok _ -> false
  | Error (`Msg e) -> raise (Error e)

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

let is_tracked_by_git ?cwd file =
  let cwd =
    match cwd with
    | None -> Some (Fpath.parent file)
    | Some _ -> cwd
  in
  let cmd = (git, cd cwd @ [ "ls-files"; "--error-unmatch"; !!file ]) in
  match UCmd.status_of_run ~quiet:true cmd with
  | Ok (`Exited 0) -> true
  | Ok _ -> false
  | Error (`Msg e) -> raise (Error e)

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

let add ?cwd files =
  let files = List_.map Fpath.to_string files in
  let cmd = (git, cd cwd @ [ "add" ] @ files) in
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

(* TODO: should return Uri.t option *)
let get_project_url ?cwd () : string option =
  let cmd = (git, cd cwd @ [ "ls-remote"; "--get-url" ]) in
  match UCmd.string_of_run ~trim:true cmd with
  | Ok (url, _) -> Some url
  | Error _ ->
      UFile.find_first_match_with_whole_line (Fpath.v ".git/config") ".com"
(* TODO(dinosaure): this line is pretty weak due to the [".com"] (what happens
   when the domain is [".io"]?). We probably should handle that by a new
   environment variable. I just copied what [pysemgrep] does.
   [git ls-remote --get-url] is also enough and if we can not get such
   information, that's fine - the metadata is used only [Metrics_] actually. *)

(* coupling: with semgrep_output_v1.atd contribution type *)
let git_log_json_format =
  "--pretty=format:{\"commit_hash\": \"%H\", \"commit_timestamp\": \"%ai\", \
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

let cat_file_batch_check_all_objects ?cwd () =
  let cmd =
    ( git,
      ("cat-file" :: cd cwd)
      @ [
          "--batch-all-objects";
          "--batch-check";
          "--unordered" (* List in pack order instead of hash; faster. *);
        ] )
  in
  let* objects =
    match UCmd.lines_of_run ~trim:true cmd with
    | Ok (s, (_, `Exited 0)) -> Some s
    | _ -> None
  in
  let objects : batch_check_extra obj list =
    List.filter_map
      (fun obj ->
        let parsed_obj =
          match String.split_on_char ' ' obj with
          | [ sha; "tag"; size ] ->
              let* size = int_of_string_opt size in
              Some { kind = Tag; sha; extra = { size } }
          | [ sha; "commit"; size ] ->
              let* size = int_of_string_opt size in
              Some { kind = Commit; sha; extra = { size } }
          | [ sha; "tree"; size ] ->
              let* size = int_of_string_opt size in
              Some { kind = Tree; sha; extra = { size } }
          | [ sha; "blob"; size ] ->
              let* size = int_of_string_opt size in
              Some { kind = Blob; sha; extra = { size } }
          | _ -> None
        in
        if Option.is_none parsed_obj then
          Logs.warn (fun m ->
              m "Issue parsing git object: %s; this object will be ignored" obj);
        parsed_obj)
      objects
  in
  Some objects

let cat_file_blob sha =
  let cmd = (git, [ "cat-file"; "blob"; sha ]) in
  match UCmd.string_of_run ~trim:false cmd with
  | Ok (s, (_, `Exited 0)) -> Ok s
  | Ok (s, _)
  | Error (`Msg s) ->
      Error s

let ls_tree ?cwd ?(recurse = false) sha : ls_tree_extra obj list option =
  let cmd =
    ( git,
      ("ls-tree" :: cd cwd)
      @ (if recurse then [ "-r" ] else [])
      @ [ "--full-tree"; "--format=%(objecttype) %(objectname) %(path)"; sha ]
    )
  in
  let* objects =
    match UCmd.lines_of_run ~trim:true cmd with
    | Ok (s, (_, `Exited 0)) -> Some s
    | _ -> None
  in
  let objects =
    List.filter_map
      (fun obj ->
        let parsed_obj =
          match String.split_on_char ' ' obj with
          | [ "commit"; sha; path ] -> (
              (* possible for submodules *)
              match Fpath.of_string path with
              | Ok path -> Ok { kind = Commit; sha; extra = { path } }
              | Error (`Msg s) -> Error s)
          | [ "tree"; sha; path ] -> (
              match Fpath.of_string path with
              | Ok path -> Ok { kind = Tree; sha; extra = { path } }
              | Error (`Msg s) -> Error s)
          | [ "blob"; sha; path ] -> (
              match Fpath.of_string path with
              | Ok path -> Ok { kind = Blob; sha; extra = { path } }
              | Error (`Msg s) -> Error s)
          | [ "tag"; sha; path ] -> (
              (* possible, but we probably don't care. *)
              match Fpath.of_string path with
              | Ok path -> Ok { kind = Tag; sha; extra = { path } }
              | Error (`Msg s) -> Error s)
          | _ -> Error "invalid syntax"
        in
        match parsed_obj with
        | Ok obj -> Some obj
        | Error s ->
            Logs.warn (fun m ->
                m
                  "Issue parsing git object: %s -- %s; this object will be \
                   ignored"
                  obj s);
            None)
      objects
  in
  Some objects
