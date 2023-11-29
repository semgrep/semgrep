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
let getcwd () = USys.getcwd () |> Fpath.v

(*
   Create an optional -C option to change directory.

   Usage:

     let cmd = Bos.Cmd.(v "git" %% cd cwd % "rev-parse" % "--show-toplevel") in
     ...
*)
let cd opt_cwd =
  match opt_cwd with
  | None -> Cmd.empty
  | Some path -> Cmd.of_list [ "-C"; !!path ]

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

let git_check_output (cmd : Cmd.t) : string =
  let out = UCmd.run_out cmd in
  match UCmd.out_string ~trim:true out with
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
      Logs.warn (fun m -> m fmt Cmd.pp cmd);
      raise (Error "Error when we run a git command")

type ls_files_kind =
  | Cached (* --cached, the default *)
  | Others (* --others, the complement of Cached but still excluding .git/ *)

let string_of_ls_files_kind (kind : ls_files_kind) =
  match kind with
  | Cached -> "--cached"
  | Others -> "--others"

let ls_files ?(cwd = Fpath.v ".") ?(kinds = []) root_paths =
  let roots = root_paths |> List_.map Fpath.to_string |> Cmd.of_list in
  let kinds = kinds |> List_.map string_of_ls_files_kind |> Cmd.of_list in
  let cmd = Cmd.(v "git" % "-C" % !!cwd % "ls-files" %% kinds %% roots) in
  Logs.info (fun m -> m "Running external command: %s" (Cmd.to_string cmd));
  let files_r = UCmd.run_out cmd in
  let results = UCmd.out_lines ~trim:true files_r in
  let files =
    match results with
    | Ok (files, (_, `Exited 0)) -> files
    | _ -> raise (Error "Could not get files from git ls-files")
  in
  files |> Fpath_.of_strings

let get_project_root ?cwd () =
  let cmd = Cmd.(v "git" %% cd cwd % "rev-parse" % "--show-toplevel") in
  let path_r = UCmd.run_out cmd in
  let result = UCmd.out_string ~trim:true path_r in
  match result with
  | Ok (path, (_, `Exited 0)) -> Some (Fpath.v path)
  | _ -> None

let get_superproject_root ?cwd () =
  let cmd =
    Cmd.(v "git" %% cd cwd % "rev-parse" % "--show-superproject-working-tree")
  in
  let path_r = UCmd.run_out cmd in
  let result = UCmd.out_string ~trim:true path_r in
  match result with
  | Ok ("", (_, `Exited 0)) -> get_project_root ?cwd ()
  | Ok (path, (_, `Exited 0)) -> Some (Fpath.v path)
  | _ -> None

let get_project_root_exn () =
  match get_project_root () with
  | Some path -> path
  | _ -> raise (Error "Could not get git root from git rev-parse")

let get_merge_base commit =
  let cmd = Cmd.(v "git" % "merge-base" % commit % "HEAD") in
  let base_r = UCmd.run_out cmd in
  let results = UCmd.out_string ~trim:true base_r in
  match results with
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
  let cmd =
    match branch with
    | None -> Cmd.(v "git" % "worktree" % "add" % temp_dir % commit)
    | Some new_branch ->
        Cmd.(
          v "git" % "worktree" % "add" % temp_dir % commit % "-b" % new_branch)
  in
  let status = UCmd.run_status ~quiet:true cmd in
  match status with
  | Ok (`Exited 0) ->
      let work () =
        Fpath.append (Fpath.v temp_dir) relative_path
        |> Fpath.to_string |> UUnix.chdir;
        f ()
      in
      let cleanup () =
        cwd |> Fpath.to_string |> UUnix.chdir;
        let cmd = Cmd.(v "git" % "worktree" % "remove" % temp_dir) in
        let status = UCmd.run_status ~quiet:true cmd in
        match status with
        | Ok (`Exited 0) -> logger#info "Finished cleaning up git worktree"
        | Ok _ -> raise (Error ("Could not remove git worktree at " ^ temp_dir))
        | Error (`Msg e) -> raise (Error e)
      in
      Common.protect ~finally:cleanup work
  | Ok _ -> raise (Error ("Could not create git worktree for " ^ commit))
  | Error (`Msg e) -> raise (Error e)

let status ~cwd ~commit =
  let cmd =
    Cmd.(
      v "git" % "-C" % !!cwd % "diff" % "--cached" % "--name-status"
      % "--no-ext-diff" % "-z" % "--diff-filter=ACDMRTUXB"
      % "--ignore-submodules" % "--relative" % commit)
  in
  let files_r = UCmd.run_out cmd in
  let results = UCmd.out_string ~trim:true files_r in
  let stats =
    match results with
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

let is_git_repo cwd =
  let cmd =
    Cmd.(v "git" % "-C" % !!cwd % "rev-parse" % "--is-inside-work-tree")
  in
  let run = UCmd.run_status ~quiet:true cmd in
  match run with
  | Ok (`Exited 0) -> true
  | Ok _ -> false
  | Error (`Msg e) -> raise (Error e)

let dirty_lines_of_file ?(git_ref = "HEAD") file =
  let cwd = Fpath.parent file in
  let cmd =
    (* --error-unmatch Returns a non 0 exit code if a file is not tracked by git. This way further on in this function we don't try running git diff on untracked files, as this isn't allowed. *)
    Cmd.(v "git" % "-C" % !!cwd % "ls-files" % "--error-unmatch" % !!file)
  in
  let status = UCmd.run_status ~quiet:true cmd in
  match (status, git_ref = "HEAD") with
  | _, false
  | Ok (`Exited 0), _ ->
      let cmd =
        Cmd.(v "git" % "-C" % !!cwd % "diff" % "-U0" % git_ref % !!file)
      in
      let out = UCmd.run_out cmd in
      let lines_r = UCmd.out_string ~trim:true out in
      let lines =
        match lines_r with
        | Ok (lines, (_, `Exited 1))
        (* 1 happens if git doesn't exit cleanly aka file is not in the repo *)
        | Ok (lines, (_, `Exited 0)) ->
            Some lines
        | _ -> None
      in
      Option.bind lines (fun l -> Some (range_of_git_diff l))
  | Ok _, _ -> None
  | Error (`Msg e), _ -> raise (Error e)

let is_tracked_by_git file =
  let cwd = Fpath.parent file in
  let cmd =
    Cmd.(v "git" % "-C" % !!cwd % "ls-files" % "--error-unmatch" % !!file)
  in
  let status = UCmd.run_status ~quiet:true cmd in
  match status with
  | Ok (`Exited 0) -> true
  | Ok _ -> false
  | Error (`Msg e) -> raise (Error e)

let dirty_files cwd =
  let cmd =
    Cmd.(
      v "git" % "-C" % !!cwd % "status" % "--porcelain" % "--ignore-submodules")
  in
  let lines_r = UCmd.run_out cmd in
  let lines = UCmd.out_lines ~trim:false lines_r in
  let lines =
    match lines with
    | Ok (lines, (_, `Exited 0)) -> lines
    | _ -> []
  in
  (* out_lines splits on newlines, so we always have an extra space at the end *)
  let files = List.filter (fun f -> not (String.trim f = "")) lines in
  let files = List_.map (fun l -> Fpath.v (Str.string_after l 3)) files in
  files

let init cwd =
  let cmd = Cmd.(v "git" % "-C" % !!cwd % "init") in
  match UCmd.run_status cmd with
  | Ok (`Exited 0) -> ()
  | _ -> raise (Error "Error running git init")

let add cwd files =
  let files = List_.map Fpath.to_string files in
  let files = String.concat " " files in
  let cmd = Cmd.(v "git" % "-C" % !!cwd % "add" % files) in
  match UCmd.run_status cmd with
  | Ok (`Exited 0) -> ()
  | _ -> raise (Error "Error running git add")

let commit cwd msg =
  let cmd = Cmd.(v "git" % "-C" % !!cwd % "commit" % "-m" % msg) in
  match UCmd.run_status cmd with
  | Ok (`Exited 0) -> ()
  | Ok (`Exited i) ->
      raise (Error (Common.spf "Error running git commit: bad exit %d" i))
  | Ok (`Signaled i) ->
      raise (Error (Common.spf "Error running git commit: bad signal %d" i))
  | Error (`Msg s) ->
      raise (Error (Common.spf "Error running git commit: %s" s))

(* TODO: should return Uri.t option *)
let get_project_url () : string option =
  let cmd = Cmd.(v "git" % "ls-remote" % "--get-url") in
  let out = UCmd.run_out cmd in
  match UCmd.out_string ~trim:true out with
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
let get_git_logs ?(since = None) () : string list =
  let cmd =
    match since with
    | None -> Cmd.(v "git" % "log" % git_log_json_format)
    | Some time ->
        let after = spf "--after=\"%s\"" (time_to_str time) in
        Cmd.(v "git" % "log" % after % git_log_json_format)
  in
  let lines_r = UCmd.run_out cmd in
  let lines = UCmd.out_lines ~trim:true lines_r in
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
