open Common
open File.Operators
module In = Input_to_core_t
module Out = Output_from_core_t
module Resp = Output_from_core_t

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(*
   Find target candidates.

   Performance: collecting target candidates is a one-time operation
   that can be relatively expensive (O(number of files)).

   Partially translated from target_manager.py
*)

(* python comment:

   Assumes file system does not change during it's existence to cache
   files for a given language etc. If file system changes (i.e. git checkout),
   create a new TargetManager object

   If respect_git_ignore is true then will only consider files that are
   tracked or (untracked but not ignored) by git

   If git_baseline_commit is true then will only consider files that have
   changed since that commit

   If allow_unknown_extensions is set then targets with extensions that are
   not understood by semgrep will always be returned by get_files. Else will
   discard targets with unknown extensions
*)

(*************************************************************************)
(* Types *)
(*************************************************************************)

(* TODO? process also user's gitignore file like ripgrep does?
 * TODO? use Glob.Pattern.t below instead of string for exclude and include_?
 *)
type conf = {
  (* global exclude list, passed via semgrep --exclude *)
  exclude : string list;
  (* global include list, passed via semgrep --include
   * [!] include_ = None is the opposite of Some [].
   * If a list of include patterns is specified, a path must match
   * at least of the patterns to be selected.
   * (--require would be a better flag name, but both grep and ripgrep
   * uses the --exclude and --include names).
   *)
  include_ : string list option;
  max_target_bytes : int;
  (* Whether or not follow what is specified in the .gitignore
   * The .semgrepignore are always respected.
   *)
  respect_git_ignore : bool;
  (* TODO? use, and better parsing of the string? a Git.version type? *)
  baseline_commit : string option;
  (* TODO: use *)
  scan_unknown_extensions : bool;
  (* osemgrep-only: option (see Git_project.ml and the force_root parameter) *)
  project_root : Fpath.t option;
}
[@@deriving show]

(* For gitignore computation, we need to operate on Ppath (see
 * the signature of Gitignore_filter.select()), but when semgrep
 * displays findings or errors, we want filenames derived from
 * the scanning roots, not the root of the project. This is why we need to
 * keep both the fpath and ppath for each target file.
 *)
type fppath = { fpath : Fpath.t; ppath : Ppath.t }

(* TODO? could move in Project.ml *)
type project_roots = {
  project : Project.t;
  (* scanning roots that belong to the project *)
  scanning_roots : fppath list;
}

(*************************************************************************)
(* Diagnostic *)
(*************************************************************************)

let get_reason_for_exclusion sel_events =
  let fallback : Resp.skip_reason = Semgrepignore_patterns_match in
  match (sel_events : Gitignore.selection_event list) with
  | Selected loc :: _ -> (
      match loc.source_kind with
      | Some str -> (
          match str with
          | "include" -> Resp.Cli_include_flags_do_not_match
          | "exclude" -> Resp.Cli_exclude_flags_match
          | "gitignore"
          | "semgrepignore" ->
              Resp.Semgrepignore_patterns_match
          | __ -> (* shouldn't happen *) fallback)
      | None -> (* shouldn't happen *) fallback)
  | Deselected _ :: _
  | [] ->
      (* shouldn't happen *) fallback

(*************************************************************************)
(* Finding (new) *)
(*************************************************************************)

(* We used to call 'git ls-files' when conf.respect_git_ignore was true,
 * which could potentially speedup things because git may rely on
 * internal data-structures to answer the question instead of walking
 * the filesystem and read the potentially many .gitignore files.
 * However this was not handling .semgrepignore and especially the
 * ability to negate gitignore decisions in a .semgrepignore, so I think it's
 * simpler to just walk the filesystem whatever the value of
 * conf.respect_git_ignore. That's what ripgrep does too.
 *
 * old:
 *   (* LATER: maybe we should first check whether scan_root is inside
 *    * a git repository because respect_git_ignore is set to true by default
 *    * and so it does not really mean the user want to use git (and
 *    * a possible .gitignore) to list files.
 *    *)
 *   if conf.respect_git_ignore then (
 *     try files_from_git_ls ~cwd:scan_root.fpath with
 *    | (Git_wrapper.Error _ | Common.CmdError _ | Unix.Unix_error _) as exn ->
 *         Logs.info (fun m ->
 *             m
 *               "Unable to ignore files ignored by git (%s is not a git \
 *                directory or git is not installed). Running on all files \
 *                instead..."
 *               !!(scan_root.fpath));
 *         Logs.debug (fun m -> m "exn = %s" (Common.exn_to_s exn));
 *         List_files.list_regular_files ~keep_root:true scan_root.fpath)
 *
 * pre: the scan_root must be a path to a directory
 * python: was called Target.files_from_filesystem ()
 *)
let walk_skip_and_collect (conf : conf) (ign : Semgrepignore.t)
    (scan_root : fppath) : Fpath.t list * Out.skipped_target list =
  (* Imperative style! walk and collect *)
  let (res : Fpath.t list ref) = ref [] in
  let (skipped : Out.skipped_target list ref) = ref [] in

  (* mostly a copy-paste of List_files.list_regular_files() *)
  let rec aux (dir : fppath) =
    Logs.debug (fun m ->
        m "listing dir %s (ppath = %s)" !!(dir.fpath)
          (Ppath.to_string dir.ppath));
    (* TODO? should we sort them first? *)
    let entries = List_files.read_dir_entries dir.fpath in
    entries
    |> List.iter (fun name ->
           let fpath =
             (* if scan_root was "." we want to display paths as "foo/bar"
              * and not "./foo/bar"
              *)
             if Fpath.equal dir.fpath (Fpath.v ".") then Fpath.v name
             else Fpath.add_seg dir.fpath name
           in
           let ppath = Ppath.add_seg dir.ppath name in
           (* TODO? if a dir, then add trailing / to ppath? it will be detected
            * though in the children of the dir at least
            *)
           (* skipping hidden files (this includes big directories like .git/)
            * TODO? maybe add a setting in conf?
            * TODO? add a skip reason for those?
            *)
           if name =~ "^\\." then ignore ()
           else
             let status, selection_events = Semgrepignore.select ign ppath in
             match status with
             | Ignored ->
                 Logs.debug (fun m ->
                     m "Ignoring path %s:\n%s" !!fpath
                       (Gitignore.show_selection_events selection_events));
                 let reason = get_reason_for_exclusion selection_events in
                 let skip =
                   {
                     Resp.path = !!fpath;
                     reason;
                     details =
                       "excluded by --include/--exclude, gitignore, or \
                        semgrepignore";
                     rule_id = None;
                   }
                 in
                 Common.push skip skipped
             | Not_ignored -> (
                 match Unix.lstat !!fpath with
                 (* skipping symlinks *)
                 | { Unix.st_kind = S_LNK; _ } -> ()
                 | { Unix.st_kind = S_REG; _ } -> Common.push fpath res
                 | { Unix.st_kind = S_DIR; _ } ->
                     (* skipping submodules.
                      * TODO? should we add a skip_reason for it? pysemgrep
                      * though was using `git ls-files` which implicitely does
                      * not even consider submodule files, so those files/dirs
                      * were not mentioned in the skip list
                      *)
                     if
                       conf.respect_git_ignore
                       && Git_project.is_git_submodule_root fpath
                     then ignore ()
                     else aux { fpath; ppath }
                 | { Unix.st_kind = S_FIFO | S_CHR | S_BLK | S_SOCK; _ } -> ()
                 (* ignore for now errors. TODO? return a skip? *)
                 | exception Unix.Unix_error (_err, _fun, _info) -> ()))
  in
  aux scan_root;
  (* TODO? List.rev? anyway we gonna sort those files later no? or for
   * displaying matches incrementally the order matters?
   *)
  (!res, !skipped)

(*************************************************************************)
(* Grouping (new) *)
(*************************************************************************)

(*
   Identify the project root for each scanning root and group them
   by project root. If the project_root is specified, then we use that.

   This is important to avoid reading the gitignore and semgrepignore files
   twice when multiple scanning roots that belong to the same project.

   TODO? move in paths/Project.ml?
*)
let group_scanning_roots_by_project (conf : conf)
    (scanning_roots : Fpath.t list) : project_roots list =
  let force_root =
    match conf.project_root with
    | None -> None
    | Some proj_root -> Some (Project.Git_project, proj_root)
  in
  scanning_roots
  |> Common.map (fun scanning_root ->
         let kind, project_root, scanning_root_ppath =
           Git_project.find_any_project_root ?force_root scanning_root
         in
         ( (kind, Rpath.of_fpath project_root),
           { fpath = scanning_root; ppath = scanning_root_ppath } ))
  (* using an Rpath in Project.t ensures we group correctly even
   * if the scanning_roots went through different symlink paths
   *)
  |> Common.group_by fst
  |> Common.map (fun (project, xs) ->
         { project; scanning_roots = xs |> Common.map snd })

(*************************************************************************)
(* Entry point (new) *)
(*************************************************************************)

let get_targets conf scanning_roots =
  scanning_roots
  |> group_scanning_roots_by_project conf
  |> List.concat_map (fun { project = kind, project_root; scanning_roots } ->
         (* step1: filter with .gitignore and .semgrepignore *)
         let exclusion_mechanism : Semgrepignore.exclusion_mechanism =
           match (kind : Project.kind) with
           | Project.Git_project ->
               if conf.respect_git_ignore then Gitignore_and_semgrepignore
               else Only_semgrepignore
           | Project.Other_project -> Only_semgrepignore
         in
         (* step2: filter also the --include and --exclude from the CLI args
          * (the paths: exclude: include: in a rule are handled elsewhere, in
          * Run_semgrep.ml by calling Filter_target.filter_paths
          *)
         let ign =
           Semgrepignore.create ?include_patterns:conf.include_
             ~cli_patterns:conf.exclude ~exclusion_mechanism
             ~project_root:(Rpath.to_fpath project_root)
             ()
         in
         scanning_roots
         |> Common.map (fun scan_root ->
                (* better: Note that we use Unix.stat below, not Unix.lstat, so
                 * osemgrep accepts symlink paths on the command--line;
                 * you can do 'osemgrep -e ... ~/symlink-to-proj' or even
                 * 'osemgrep -e ... symlink-to-file.py' whereas pysemgrep
                 * exits with '"/home/foo/symlink-to-proj" file not found'
                 * Note: This may raise Unix.Unix_error.
                 * TODO? improve Unix.Unix_error in Find_targets specific exn?
                 *)
                match (Unix.stat !!(scan_root.fpath)).st_kind with
                (* TOPORT? make sure has right permissions (readable) *)
                | S_REG -> ([ scan_root.fpath ], [])
                | S_DIR -> walk_skip_and_collect conf ign scan_root
                | S_LNK ->
                    (* already dereferenced by Unix.stat *)
                    raise Impossible
                (* TODO? use write_pipe_to_disk? *)
                | S_FIFO -> ([], [])
                (* TODO? return an error message or a new skipped_target kind? *)
                | S_CHR
                | S_BLK
                | S_SOCK ->
                    ([], [])))
  |> List.split
  |> fun (paths_list, skipped_paths_list) ->
  (List.flatten paths_list, List.flatten skipped_paths_list)
  [@@profiling]

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

let sort_targets_by_decreasing_size targets =
  targets
  |> Common.map (fun target -> (target, Common2.filesize target.In.path))
  |> List.sort (fun (_, (a : int)) (_, b) -> compare b a)
  |> Common.map fst

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
let get_targets2 conf scanning_roots =
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
                  let git_path = Ppath.(of_fpath rel_path |> make_absolute) in
                  let status, selection_events =
                    Semgrepignore.select ign git_path
                  in
                  match status with
                  | Not_ignored -> Left path
                  | Ignored ->
                      Logs.debug (fun m ->
                          m "Ignoring path %s:\n%s" !!path
                            (Gitignore.show_selection_events selection_events));
                      let reason = get_reason_for_exclusion selection_events in
                      let skipped =
                        {
                          Resp.path = !!path;
                          reason;
                          details =
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
                          spf "target file size exceeds %i bytes at %i bytes"
                            conf.max_target_bytes size;
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
