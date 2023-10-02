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
open File.Operators

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
(* Types *)
(*****************************************************************************)

type status = {
  added : string list;
  modified : string list;
  removed : string list;
  unmerged : string list;
  renamed : (string * string) list;
}
[@@deriving show]

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
let git_diff_lines_re = SPcre.regexp _git_diff_lines_re

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
  let matched_ranges = SPcre.exec_all ~rex:git_diff_lines_re lines in
  (* get the first capture group, then optionally split the comma if multiline diff *)
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

let git_check_output (cmd : Bos.Cmd.t) : string =
  let out = Bos.OS.Cmd.run_out cmd in
  match Bos.OS.Cmd.out_string ~trim:true out with
  | Ok (str, (_, `Exited 0)) -> str
  | Ok _
  | Error (`Msg _) ->
      let fmt : _ format4 =
        {|Command failed.
-----
Failed to run %a. Possible reasons:
- the git binary is not available
- the current working directory is not a git repository
- the current working directory is not marked as safe
  (fix with `git config --global --add safe.directory $(pwd)`)

Try running the command yourself to debug the issue.|}
      in
      Logs.warn (fun m -> m fmt Bos.Cmd.pp cmd);
      raise (Error "Error when we run a git command")

let files_from_git_ls ~cwd =
  let cmd = Bos.Cmd.(v "git" % "-C" % !!cwd % "ls-files") in
  let files_r = Bos.OS.Cmd.run_out cmd in
  let results = Bos.OS.Cmd.out_lines ~trim:true files_r in
  let files =
    match results with
    | Ok (files, (_, `Exited 0)) -> files
    | _ -> raise (Error "Could not get files from git ls-files")
  in
  files |> File.Path.of_strings

let get_git_root_path () =
  let cmd = Bos.Cmd.(v "git" % "rev-parse" % "--show-toplevel") in
  let path_r = Bos.OS.Cmd.run_out cmd in
  let result = Bos.OS.Cmd.out_string ~trim:true path_r in
  match result with
  | Ok (path, (_, `Exited 0)) -> path
  | _ -> raise (Error "Could not get git root from git rev-parse")

let get_merge_base commit =
  let cmd = Bos.Cmd.(v "git" % "merge-base" % commit % "HEAD") in
  let base_r = Bos.OS.Cmd.run_out cmd in
  let results = Bos.OS.Cmd.out_string ~trim:true base_r in
  match results with
  | Ok (merge_base, (_, `Exited 0)) -> merge_base
  | _ -> raise (Error "Could not get merge base from git merge-base")

let run_with_worktree ~commit ?(branch = None) f =
  let cwd = Sys.getcwd () |> Fpath.v |> Fpath.to_dir_path in
  let git_root = get_git_root_path () |> Fpath.v |> Fpath.to_dir_path in
  let relative_path =
    match Fpath.relativize ~root:git_root cwd with
    | Some p -> p
    | None -> raise (Error "")
  in
  let rand_dir () =
    let uuid = Uuidm.v `V4 in
    let dir_name = "semgrep_git_worktree_" ^ Uuidm.to_string uuid in
    let dir = Filename.concat (Filename.get_temp_dir_name ()) dir_name in
    Unix.mkdir dir 0o777;
    dir
  in
  let temp_dir = rand_dir () in
  let cmd =
    match branch with
    | None -> Bos.Cmd.(v "git" % "worktree" % "add" % temp_dir % commit)
    | Some new_branch ->
        Bos.Cmd.(
          v "git" % "worktree" % "add" % temp_dir % commit % "-b" % new_branch)
  in
  let status = Bos.OS.Cmd.run_status ~quiet:true cmd in
  match status with
  | Ok (`Exited 0) ->
      let work () =
        Fpath.append (Fpath.v temp_dir) relative_path
        |> Fpath.to_string |> Unix.chdir;
        f ()
      in
      let cleanup () =
        cwd |> Fpath.to_string |> Unix.chdir;
        let cmd = Bos.Cmd.(v "git" % "worktree" % "remove" % temp_dir) in
        let status = Bos.OS.Cmd.run_status ~quiet:true cmd in
        match status with
        | Ok (`Exited 0) -> logger#info "Finished cleaning up git worktree"
        | Ok _ -> raise (Error ("Could not remove git worktree at " ^ temp_dir))
        | Error (`Msg e) -> raise (Error e)
      in
      Fun.protect ~finally:cleanup work
  | Ok _ -> raise (Error ("Could not create git worktree for " ^ commit))
  | Error (`Msg e) -> raise (Error e)

let status ~cwd ~commit =
  let cmd =
    Bos.Cmd.(
      v "git" % "-C" % !!cwd % "diff" % "--cached" % "--name-status"
      % "--no-ext-diff" % "-z" % "--diff-filter=ACDMRTUXB"
      % "--ignore-submodules" % "--relative" % commit)
  in
  let files_r = Bos.OS.Cmd.run_out cmd in
  let results = Bos.OS.Cmd.out_string ~trim:true files_r in
  let stats =
    match results with
    | Ok (str, (_, `Exited 0)) -> str |> String.split_on_char '\000'
    | _ -> raise (Error "Could not get files from git ls-files")
  in
  let check_dir file =
    try
      match (Unix.stat file).st_kind with
      | Unix.S_DIR -> true
      | _ -> false
    with
    | _ -> false
  in
  let check_symlink file =
    try
      match (Unix.lstat file).st_kind with
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
          (Unix.realpath file);
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

let is_git_repo cwd =
  let cmd =
    Bos.Cmd.(v "git" % "-C" % !!cwd % "rev-parse" % "--is-inside-work-tree")
  in
  let run = Bos.OS.Cmd.run_status ~quiet:true cmd in
  match run with
  | Ok (`Exited 0) -> true
  | Ok _ -> false
  | Error (`Msg e) -> raise (Error e)

let dirty_lines_of_file ?(git_ref = "HEAD") file =
  let cwd = Fpath.parent file in
  let cmd =
    Bos.Cmd.(v "git" % "-C" % !!cwd % "ls-files" % "--error-unmatch" % !!file)
  in
  let status = Bos.OS.Cmd.run_status ~quiet:true cmd in
  match (status, git_ref = "HEAD") with
  | _, false
  | Ok (`Exited 0), _ ->
      let cmd =
        Bos.Cmd.(v "git" % "-C" % !!cwd % "diff" % "-U0" % git_ref % !!file)
      in
      let out = Bos.OS.Cmd.run_out cmd in
      let lines_r = Bos.OS.Cmd.out_string ~trim:true out in
      let lines =
        match lines_r with
        | Ok (lines, (_, `Exited 1))
        (* 1 happens if git doesn't exit cleanly aka file is not in the repo *)
        | Ok (lines, (_, `Exited 0)) ->
            Logs.app (fun m -> m "GIT DIFF lines: %s" lines);
            Some lines
        | _ -> None
      in
      Option.bind lines (fun l -> Some (range_of_git_diff l))
  | Ok _, _ -> None
  | Error (`Msg e), _ -> raise (Error e)

let is_tracked_by_git file =
  let cwd = Fpath.parent file in
  let cmd =
    Bos.Cmd.(v "git" % "-C" % !!cwd % "ls-files" % "--error-unmatch" % !!file)
  in
  let status = Bos.OS.Cmd.run_status ~quiet:true cmd in
  match status with
  | Ok (`Exited 0) -> true
  | Ok _ -> false
  | Error (`Msg e) -> raise (Error e)

let dirty_files cwd =
  let cmd =
    Bos.Cmd.(
      v "git" % "-C" % !!cwd % "status" % "--porcelain" % "--ignore-submodules")
  in
  let lines_r = Bos.OS.Cmd.run_out cmd in
  let lines = Bos.OS.Cmd.out_lines ~trim:false lines_r in
  let lines =
    match lines with
    | Ok (lines, (_, `Exited 0)) -> lines
    | _ -> []
  in
  (* out_lines splits on newlines, so we always have an extra space at the end *)
  let files = List.filter (fun f -> not (String.trim f = "")) lines in
  let files = Common.map (fun l -> Fpath.v (Str.string_after l 3)) files in
  files

let init cwd =
  let cmd = Bos.Cmd.(v "git" % "-C" % !!cwd % "init") in
  match Bos.OS.Cmd.run_status cmd with
  | Ok (`Exited 0) -> ()
  | _ -> raise (Error "Error running git init")

let add cwd files =
  let files = Common.map Fpath.to_string files in
  let files = String.concat " " files in
  let cmd = Bos.Cmd.(v "git" % "-C" % !!cwd % "add" % files) in
  match Bos.OS.Cmd.run_status cmd with
  | Ok (`Exited 0) -> ()
  | _ -> raise (Error "Error running git add")

let commit cwd msg =
  let cmd = Bos.Cmd.(v "git" % "-C" % !!cwd % "commit" % "-m" % msg) in
  match Bos.OS.Cmd.run_status cmd with
  | Ok (`Exited 0) -> ()
  | _ -> raise (Error "Error running git commit")

(* TODO: should return Uri.t option *)
let get_project_url () : string option =
  let cmd = Bos.Cmd.(v "git" % "ls-remote" % "--get-url") in
  let out = Bos.OS.Cmd.run_out cmd in
  match Bos.OS.Cmd.out_string ~trim:true out with
  | Ok (url, _) -> Some url
  | Error _ ->
      File.find_first_match_with_whole_line (Fpath.v ".git/config") ".com"
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

let time_to_str (timestamp : Common2.float_time) : string =
  let date = Unix.gmtime timestamp in
  let year = date.tm_year + 1900 in
  let month = date.tm_mon + 1 in
  let day = date.tm_mday in
  Printf.sprintf "%04d-%02d-%02d" year month day

(* TODO: should really return a JSON.t list at least *)
let get_git_logs ?(since = None) () : string list =
  let cmd =
    match since with
    | None -> Bos.Cmd.(v "git" % "log" % git_log_json_format)
    | Some time ->
        let after = spf "--after=\"%s\"" (time_to_str time) in
        Bos.Cmd.(v "git" % "log" % after % git_log_json_format)
  in
  let lines_r = Bos.OS.Cmd.run_out cmd in
  let lines = Bos.OS.Cmd.out_lines ~trim:true lines_r in
  let lines =
    match lines with
    (* ugly: we should parse those lines and return a proper type,
     * at least a JSON.t.
     *)
    | Ok (lines, (_, `Exited 0)) -> lines
    (* TODO: maybe should raise an Error instead *)
    | _ -> []
  in
  (* out_lines splits on newlines, so we always have an extra space at the end *)
  List.filter (fun f -> not (String.trim f = "")) lines
