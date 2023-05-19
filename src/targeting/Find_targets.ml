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
(* Finding *)
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
(* Grouping *)
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
(* Entry point *)
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
          *
          * We currently handle gitignores by creating this
          * ign below that then will internally use some cache and complex
          * logic to select files in walk_skip_and_collect().
          * TODO? we could instead change strategy and accumulate the
          * current set of applicable gitignore as we walk down the FS
          * hierarchy. We would not need then to look at each element
          * in the ppath and look for the present of a .gitignore there;
          * the job would have already been done as we walked!
          * We would still need to intialize at the beginning with
          * the .gitignore of all the parents of the scan_root.
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
