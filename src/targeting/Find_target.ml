module In = Input_to_core_t
module Out = Output_from_core_t

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(*
   Find and filter targets.

   Performance: The step that collects global targets is a one-time operation
   that can be relatively expensive (O(number of files)).
   The second step is done for each pair (rule, target) and can
   become problematic since the number of such pairs is O(number of targets
   * number of rules). This is why we cache the results of this step.
   This allows reducing the number of rules to the number of different
   languages and patterns used by the rules.
 *)

(*
   Handles all file include/exclude logic for semgrep

   Assumes file system does not change during it's existence to cache
   files for a given language etc. If file system changes (i.e. git checkout),
   create a new TargetManager object

   If respect_git_ignore is true then will only consider files that are
   tracked or (untracked but not ignored) by git

   If git_baseline_commit is true then will only consider files that have
   changed since that commit

   If allow_unknown_extensions is set then targets with extensions that are
   not understood by semgrep will always be returned by get_files. Else will discard
   targets with unknown extensions

   TargetManager not to be confused with https://jobs.target.com/search-jobs/store%20manager

   Translated from target_manager.py
*)

(*************************************************************************)
(* Types *)
(*************************************************************************)

(* TODO? use Fpath library at some point? *)
type path = string

type conf = {
  exclude : string list;
  include_ : string list;
  max_target_bytes : int;
  respect_git_ignore : bool;
  (* TODO? use, and better parsing of the string? a Git.version type? *)
  baseline_commit : string option;
  (* TODO: use *)
  scan_unknown_extensions : bool;
}
[@@deriving show]

type baseline_handler = TODO
type file_ignore = TODO

(*
   Some rules will use 'include' (required_path_patterns) and 'exclude'
   (excluded_path_patterns) to select targets that don't have an extension
   such as 'Dockerfile'. We expect most rules written for a language
   to use the same combination of include/exclude. This allows caching
   across the many rules that target the same language.
*)
type target_cache_key = {
  path : path;
  lang : Xlang.t;
  required_path_patterns : string list;
  excluded_path_patterns : string list;
}

type target_cache = (target_cache_key, bool) Hashtbl.t

(*************************************************************************)
(* Finding *)
(*************************************************************************)

(* Check if file is a readable regular file.

   This eliminates files that should never be semgrep targets. Among
   others, this takes care of excluding symbolic links (because we don't
   want to scan the target twice), directories (which may be returned by
   globbing or by 'git ls-files' e.g. submodules), and
   TODO files missing the read permission.
*)
let is_valid_file file =
  (* TOPORT: return self._is_valid_file_or_dir(path) and path.is_file() *)
  Common2.is_file file

(* 'git ls-files' is significantly faster than os.walk when performed on
 * a git project, so identify the git files first, then filter those later.
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
  let tracked_output = Git.files_from_git_ls ~cwd:scan_root in
  tracked_output
  |> Common.map (fun x -> Filename.concat scan_root x)
  |> List.filter is_valid_file

(* python: mostly Target.files() method in target_manager.py *)
let list_regular_files (conf : conf) (scan_root : path) : path list =
  (* This may raise Unix.Unix_error.
   * osemgrep-new: Note that I use Unix.stat below, not Unix.lstat, so we can
   * actually analyze symlink to dirs!
   * TODO? improve Unix.Unix_error in Find_target specific exn?
   *)
  match (Unix.stat scan_root).st_kind with
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
        | (Git.Error _ | Common.CmdError _ | Unix.Unix_error _) as exn ->
            Logs.info (fun m ->
                m
                  "Unable to ignore files ignored by git (%s is not a git \
                   directory or git is not installed). Running on all files \
                   instead..."
                  scan_root);
            Logs.debug (fun m -> m "exn = %s" (Common.exn_to_s exn));
            List_files.list_regular_files ~keep_root:true scan_root)
      else
        (* python: was called Target.files_from_filesystem () *)
        List_files.list_regular_files ~keep_root:true scan_root
  (* TODO? handle symlink? python does not I think and may raise exn here
   * but I think we should, at least symlink to dirs.
   * assert false because of use of Unix.stat above now?
   *)
  | S_LNK -> []
  (* TODO? use write_pipe_to_disk? *)
  | S_FIFO -> []
  | S_CHR
  | S_BLK
  | S_SOCK ->
      []

(*************************************************************************)
(* Dedup *)
(*************************************************************************)

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
  |> Common.map (fun file -> (file, Common2.filesize file))
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
  let paths, skipped1 = Skip_target.exclude_files_in_skip_lists paths in
  let paths, skipped2 =
    match opt_lang with
    | None -> (paths, [])
    | Some lang -> Guess_lang.inspect_files lang paths
  in
  let paths, skipped3 = Skip_target.exclude_big_files paths in
  let paths, skipped4 = Skip_target.exclude_minified_files paths in
  let skipped = Common.flatten [ skipped1; skipped2; skipped3; skipped4 ] in
  let sorted_paths =
    if sort_by_decr_size then sort_files_by_decreasing_size paths else paths
  in
  let sorted_skipped =
    List.sort
      (fun (a : Out.skipped_target) b -> String.compare a.path b.path)
      skipped
  in
  (sorted_paths, sorted_skipped)

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

(* python: mix of Target_manager(), target_manager.get_files_for_rule(),
 * target_manager.get_all_files(), Target(), and Target.files() *)
let get_targets conf scanning_roots =
  (* python: =~ Target_manager.get_all_files() *)
  let paths =
    scanning_roots
    |> List.concat_map (fun scan_root ->
           list_regular_files conf (Fpath.to_string scan_root))
    |> deduplicate_list
  in
  let paths, skipped_paths =
    global_filter ~opt_lang:None ~sort_by_decr_size:true paths
  in
  (* !!!TODO!!! use conf.include_, conf.exclude_,
   * max_target_bytes (* from the semgrep CLI, not semgrep-core *)
   * respect_git_ignore, baseline_handler, file_ignore?, etc.
   *)
  (paths, skipped_paths)

(*************************************************************************)
(* TODO *)
(*************************************************************************)

let create_cache () = Hashtbl.create 1000

let match_glob_pattern ~pat path =
  (* TODO *)
  ignore pat;
  ignore path;
  true

let match_a_required_path_pattern required_path_patterns path =
  match required_path_patterns with
  | [] -> (* <grimacing face emoji> *) true
  | pats -> List.exists (fun pat -> match_glob_pattern ~pat path) pats

let match_all_excluded_path_patterns excluded_path_patterns path =
  List.for_all (fun pat -> match_glob_pattern ~pat path) excluded_path_patterns

let match_language (xlang : Xlang.t) path =
  match xlang with
  | L (lang, langs) ->
      (* ok if the file appears to be in one of rule's languages *)
      List.exists
        (fun lang -> Guess_lang.inspect_file_p lang path)
        (lang :: langs)
  | LRegex
  | LGeneric ->
      true

let filter_target_for_lang ~cache ~lang ~required_path_patterns
    ~excluded_path_patterns path =
  let key : target_cache_key =
    { path; lang; required_path_patterns; excluded_path_patterns }
  in
  match Hashtbl.find_opt cache key with
  | Some res -> res
  | None ->
      let res =
        match_a_required_path_pattern required_path_patterns path
        && match_all_excluded_path_patterns excluded_path_patterns path
        && match_language lang path
      in
      Hashtbl.replace cache key res;
      res

let filter_target_for_rule cache (rule : Rule.t) (path : path) =
  let required_path_patterns, excluded_path_patterns =
    match rule.paths with
    | Some { include_; exclude } -> (include_, exclude)
    | None -> ([], [])
  in
  filter_target_for_lang ~cache ~lang:rule.languages ~required_path_patterns
    ~excluded_path_patterns path

let filter_targets_for_rule cache rule files =
  List.filter (filter_target_for_rule cache rule) files

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
             Sys.file_exists path && not (Sys.is_directory path))
    else (roots, [])
  in
  let paths = Common.files_of_dir_or_files_no_vcs_nofilter paths in
  let paths, skipped = global_filter ~opt_lang ~sort_by_decr_size paths in
  let paths = explicit_targets @ paths in
  let sorted_paths =
    if sort_by_decr_size then sort_files_by_decreasing_size paths
    else List.sort String.compare paths
  in
  let sorted_skipped =
    List.sort
      (fun (a : Out.skipped_target) b -> String.compare a.path b.path)
      skipped
  in
  (sorted_paths, sorted_skipped)
