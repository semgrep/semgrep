open Common
open File.Operators
module In = Input_to_core_t
module Out = Semgrep_output_v1_t
module Resp = Semgrep_output_v1_t
open Find_targets (* conf type *)

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* Deprecated: use Find_targets.ml instead. *)

(*************************************************************************)
(* Dedup *)
(*************************************************************************)

(* TODO? use Common2.uniq_eff *)
let deduplicate_list l =
  let tbl = Hashtbl.create 1000 in
  List.filter
    (fun x ->
      if Hashtbl.mem tbl x then false
      else (
        Hashtbl.add tbl x ();
        true))
    l

(*************************************************************************)
(* Sorting *)
(*************************************************************************)

(* similar to Run_semgrep.sort_targets_by_decreasing_size, could factorize *)
let sort_files_by_decreasing_size files =
  files
  |> Common.map (fun file -> (file, File.filesize file))
  |> List.sort (fun (_, (a : int)) (_, b) -> compare b a)
  |> Common.map fst

(*************************************************************************)
(* Filtering *)
(*************************************************************************)

(*
   Filter files can make suitable targets, independently from specific
   rules or languages.

   'sort_by_decr_size' should always be true but we keep it as an option
   for compatibility with the legacy implementation 'files_of_dirs_or_files'.

   '?lang' is a legacy option that shouldn't be used in
   the language-independent 'select_global_targets'.
*)
let global_filter ~opt_lang ~sort_by_decr_size paths =
  let paths, skipped1 =
    match opt_lang with
    | None -> (paths, [])
    | Some lang -> Guess_lang.inspect_files lang paths
  in
  let paths, skipped2 = Skip_target.exclude_big_files paths in
  let paths, skipped3 = Skip_target.exclude_minified_files paths in
  let skipped = Common.flatten [ skipped1; skipped2; skipped3 ] in
  let sorted_paths =
    if sort_by_decr_size then sort_files_by_decreasing_size paths else paths
  in
  let sorted_skipped =
    List.sort
      (fun (a : Out.skipped_target) b -> String.compare a.path b.path)
      skipped
  in
  (sorted_paths, sorted_skipped)
  [@@profiling]

(*************************************************************************)
(* Grouping (old) *)
(*************************************************************************)

(*
   func must return:
     ((project_kind, project_root), path)
*)
let group_by_project_root func paths =
  Common.map func paths |> Common.group_by fst
  |> Common.map (fun (k, kv_list) -> (k, Common.map snd kv_list))

(*
   Identify the project root for each scanning root and group them
   by project root. If the project_root is specified, then we use that.

   This is important to avoid reading the gitignore and semgrepignore files
   twice when multiple scanning roots that belong to the same project.

   LATER: use Ppaths rather than full paths as scanning roots
   when we switch to Semgrepignore.list to list project files.

   TODO? move in paths/Project.ml?
*)
let group_roots_by_project conf paths =
  let force_root =
    match conf.project_root with
    | None -> None
    | Some proj_root -> Some (Project.Git_project, proj_root)
  in
  if conf.respect_git_ignore then
    paths
    |> group_by_project_root (fun path ->
           match Git_project.find_any_project_root ?force_root path with
           | (Project.Other_project as kind), root, git_path ->
               ((kind, root), Ppath.to_fpath root git_path)
           | (Project.Git_project as kind), root, git_path ->
               ((kind, root), Ppath.to_fpath ~root git_path))
  else
    (* ignore gitignore files but respect semgrepignore files *)
    paths
    |> group_by_project_root (fun path ->
           let root, git_path = Git_project.force_project_root path in
           ((Project.Other_project, root), Ppath.to_fpath root git_path))

(*************************************************************************)
(* Finding (old) *)
(*************************************************************************)

(* Check if file is a readable regular file.

   This eliminates files that should never be semgrep targets. Among
   others, this takes care of excluding symbolic links (because we don't
   want to scan the target twice), directories (which may be returned by
   globbing or by 'git ls-files' e.g. submodules), and
   TODO files missing the read permission.

   bugfix: we were using Common2.is_file but it is throwing an exn when a
   file does exist
   (which could happen when a file tracked by git was deleted, in which
   case files_from_git_ls() below would still return it but Common2.is_file()
   would fail).

   TODO: we could return a skipped_target if the valid is not valid, adding
   a new Unreadable_file and Inexistent_file to semgrep_output_v1.atd
   skip_reason type.
*)
let is_valid_file file =
  (* TOPORT: return self._is_valid_file_or_dir(path) and path.is_file() *)
  try
    let stat = Unix.stat !!file in
    stat.Unix.st_kind =*= Unix.S_REG
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> false

(* python: 'git ls-files' is significantly faster than os.walk when performed
 * on a git project, so identify the git files first, then filter those later.
 *)
let files_from_git_ls ~cwd:scan_root =
  (* TOPORT:
      # Untracked but not ignored files
      untracked_output = run_git_command([
              "git",
              "ls-files",
              "--other",
              "--exclude-standard",
          ])
      deleted_output = run_git_command(["git", "ls-files", "--deleted"])
      tracked = self._parse_git_output(tracked_output)
      untracked_unignored = self._parse_git_output(untracked_output)
      deleted = self._parse_git_output(deleted_output)
      return frozenset(tracked | untracked_unignored - deleted)
  *)
  (* tracked files *)
  let tracked_output = Git_wrapper.files_from_git_ls ~cwd:scan_root in
  tracked_output
  |> Common.map (fun x -> if !!scan_root = "." then x else scan_root // x)
  |> List.filter is_valid_file
  [@@profiling]

(* python: mostly Target.files() method in target_manager.py *)
let list_regular_files (conf : conf) (scan_root : Fpath.t) : Fpath.t list =
  (* This may raise Unix.Unix_error.
   * better: Note that I use Unix.stat below, not Unix.lstat, so we can
   * actually analyze symlink to dirs!
   * TODO? improve Unix.Unix_error in Find_targets specific exn?
   *)
  match (Unix.stat !!scan_root).st_kind with
  (* TOPORT? make sure has right permissions (readable) *)
  | S_REG -> [ scan_root ]
  | S_DIR ->
      (* LATER: maybe we should first check whether scan_root is inside
       * a git repository because respect_git_ignore is set to true by default
       * and so it does not really mean the user want to use git (and
       * a possible .gitignore) to list files.
       *)
      if conf.respect_git_ignore then (
        try files_from_git_ls ~cwd:scan_root with
        | (Git_wrapper.Error _ | Common.CmdError _ | Unix.Unix_error _) as exn
          ->
            Logs.info (fun m ->
                m
                  "Unable to ignore files ignored by git (%s is not a git \
                   directory or git is not installed). Running on all files \
                   instead..."
                  !!scan_root);
            Logs.debug (fun m -> m "exn = %s" (Common.exn_to_s exn));
            List_files.list_regular_files ~keep_root:true scan_root)
      else
        (* python: was called Target.files_from_filesystem () *)
        List_files.list_regular_files ~keep_root:true scan_root
  | S_LNK ->
      (* already dereferenced by Unix.stat *)
      assert false
  (* TODO? use write_pipe_to_disk? *)
  | S_FIFO -> []
  | S_CHR
  | S_BLK
  | S_SOCK ->
      []
  [@@profiling]

(*************************************************************************)
(* Entry point (old) *)
(*************************************************************************)

(* python: mix of Target_manager(), target_manager.get_files_for_rule(),
   target_manager.get_all_files(), Target(), and Target.files()

   This takes a list of scanning roots which are either regular files
   or directories. Directories are scanned and we return files discovered
   in this directories.

   See the documentation for the conf object for the various filters
   that we apply.
*)
let get_targets conf scanning_roots =
  (* python: =~ Target_manager.get_all_files() *)
  group_roots_by_project conf scanning_roots
  |> Common.map (fun ((proj_kind, project_root), scanning_roots) ->
         (* step0: starting point (git ls-files or List_files) *)
         let paths =
           scanning_roots
           |> List.concat_map (fun scan_root ->
                  list_regular_files conf scan_root)
           |> deduplicate_list
         in

         (* step1: filter with .gitignore and .semgrepignore *)
         let exclusion_mechanism : Semgrepignore.exclusion_mechanism =
           match (proj_kind : Project.kind) with
           | Project.Git_project -> Gitignore_and_semgrepignore
           | Project.Other_project -> Only_semgrepignore
         in
         let ign =
           Semgrepignore.create ?include_patterns:conf.include_
             ~cli_patterns:conf.exclude ~exclusion_mechanism ~project_root ()
         in
         let paths, skipped_paths1 =
           paths
           |> Common.partition_either (fun path ->
                  Logs.debug (fun m -> m "Considering path %s" !!path);
                  let rel_path =
                    match Fpath.relativize ~root:project_root path with
                    | Some x -> x
                    | None ->
                        (* we're supposed to be working with clean paths by now *)
                        assert false
                  in
                  let git_path =
                    let segments = Fpath.segs rel_path in
                    match Ppath.from_segments ("" :: segments) with
                    | Ok x -> x
                    (* or raise Impossible? *)
                    | Error s -> failwith s
                  in
                  let status, selection_events =
                    Semgrepignore.select ign git_path
                  in
                  match status with
                  | Not_ignored -> Left path
                  | Ignored ->
                      Logs.debug (fun m ->
                          m "Ignoring path %s:\n%s" !!path
                            (Gitignore.show_selection_events selection_events));
                      let reason =
                        Find_targets.get_reason_for_exclusion selection_events
                      in
                      let skipped =
                        {
                          Resp.path = !!path;
                          reason;
                          details =
                            Some
                              "excluded by --include/--exclude, gitignore, or \
                               semgrepignore";
                          rule_id = None;
                        }
                      in
                      Right skipped)
         in
         (* step2: other filters (big files, minified files) *)
         let paths, skipped_paths2 =
           global_filter ~opt_lang:None ~sort_by_decr_size:true paths
         in
         (* step3: filter big files (again).
          * TODO: factorize with Skip_target.exlude_big_files which
          * uses Flag_semgrep.max_target_bytes instead of the osemgrep CLI
          * --max-target-bytes flag.
          *)
         let paths, skipped_paths3 =
           paths
           |> Common.partition_result (fun path ->
                  let size = File.filesize path in
                  if conf.max_target_bytes > 0 && size > conf.max_target_bytes
                  then
                    Error
                      {
                        Resp.path = !!path;
                        reason = Too_big;
                        details =
                          Some
                            (spf "target file size exceeds %i bytes at %i bytes"
                               conf.max_target_bytes size);
                        rule_id = None;
                      }
                  else Ok path)
         in

         (* TODO: respect_git_ignore, baseline_handler, etc. *)
         (paths, skipped_paths1 @ skipped_paths2 @ skipped_paths3))
  |> (* flatten results that were grouped by project *)
  List.split
  |> fun (paths_list, skipped_paths_list) ->
  (List.flatten paths_list, List.flatten skipped_paths_list)
  [@@profiling]

(*************************************************************************)
(* Legacy *)
(*************************************************************************)

(* Legacy semgrep-core implementation, used when receiving targets from
   the Python wrapper. *)
let files_of_dirs_or_files ?(keep_root_files = true)
    ?(sort_by_decr_size = false) opt_lang roots =
  let explicit_targets, paths =
    if keep_root_files then
      roots
      |> List.partition (fun path ->
             Sys.file_exists !!path && not (Sys.is_directory !!path))
    else (roots, [])
  in
  let paths =
    paths |> File.Path.to_strings
    |> Common.files_of_dir_or_files_no_vcs_nofilter |> File.Path.of_strings
  in
  let paths, skipped = global_filter ~opt_lang ~sort_by_decr_size paths in
  let paths = explicit_targets @ paths in
  let sorted_paths =
    if sort_by_decr_size then sort_files_by_decreasing_size paths
    else List.sort Fpath.compare paths
  in
  let sorted_skipped =
    List.sort
      (fun (a : Out.skipped_target) b -> String.compare a.path b.path)
      skipped
  in
  (sorted_paths, sorted_skipped)
