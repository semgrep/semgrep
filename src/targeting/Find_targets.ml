open Common
open Fpath_.Operators
module Out = Semgrep_output_v1_t

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(*
   Find target candidates.

   Performance: collecting target candidates is a one-time operation
   that can be relatively expensive (O(number of files)).

   Partially translated from target_manager.py

   Original python comments:

     Assumes file system does not change during it's existence to cache
     files for a given language etc. If file system changes
     (i.e. git checkout), create a new TargetManager object

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

module Fppath_set = Set.Make (Fppath)

(* TODO? process also user's gitignore file like ripgrep does?
   TODO? use Glob.Pattern.t below instead of string for exclude and include_?
   TODO: add an option to select all git-tracked files regardless of
         gitignore or semgrepignore exclusions (will be needed for Secrets)
         and have the exclusions apply only to the files that aren't tracked.
*)

type git_remote = { url : Uri.t; checkout_path : Fpath.t } [@@deriving show]

type project_root = Git_remote of git_remote | Filesystem of Fpath.t
[@@deriving show]

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
  respect_gitignore : bool;
  (* TODO? use, and better parsing of the string? a Git.version type? *)
  baseline_commit : string option;
  diff_depth : int;
  (* TODO: use *)
  scan_unknown_extensions : bool;
  (* osemgrep-only: option (see Git_project.ml and the force_root parameter) *)
  project_root : project_root option;
}
[@@deriving show]

(* TODO? could move in Project.ml *)
type project_roots = {
  project : Project.t;
  (* scanning roots that belong to the project *)
  scanning_roots : Fppath.t list;
}
(*****************************************************************************)
(* Hooks *)
(*****************************************************************************)

(*************************************************************************)
(* Diagnostic *)
(*************************************************************************)

let get_reason_for_exclusion (sel_events : Gitignore.selection_event list) :
    Out.skip_reason =
  let fallback = Out.Semgrepignore_patterns_match in
  match sel_events with
  | Gitignore.Selected loc :: _ -> (
      match loc.source_kind with
      | Some str -> (
          match str with
          | "include" -> Out.Cli_include_flags_do_not_match
          | "exclude" -> Out.Cli_exclude_flags_match
          (* TODO: osemgrep supports the new Gitignore_patterns_match, but for
           * legacy reason we don't generate it for now.
           *)
          | "gitignore"
          | "semgrepignore" ->
              Out.Semgrepignore_patterns_match
          | __ -> (* shouldn't happen *) fallback)
      | None -> (* shouldn't happen *) fallback)
  | Gitignore.Deselected _ :: _
  | [] ->
      (* shouldn't happen *) fallback

(*************************************************************************)
(* Finding *)
(*************************************************************************)

type filter_result =
  | Keep (* select this target file *)
  | Dir (* the path is a directory to scan recursively *)
  | Skip of Out.skipped_target (* ignore this file and report it *)
  | Ignore_silently (* ignore and don't report this file *)

let filter_path (ign : Semgrepignore.t) (fppath : Fppath.t) : filter_result =
  let { fpath; ppath } : Fppath.t = fppath in
  let status, selection_events = Semgrepignore.select ign ppath in
  match status with
  | Ignored ->
      Logs.debug (fun m ->
          m "Ignoring path %s:\n%s" !!fpath
            (Gitignore.show_selection_events selection_events));
      let reason = get_reason_for_exclusion selection_events in
      Skip
        {
          Out.path = fpath;
          reason;
          details =
            Some "excluded by --include/--exclude, gitignore, or semgrepignore";
          rule_id = None;
        }
  | Not_ignored -> (
      (* TODO: check read permission? *)
      match Unix.lstat !!fpath with
      (* skipping symlinks *)
      | { Unix.st_kind = S_LNK; _ } -> Ignore_silently
      | { Unix.st_kind = S_REG; _ } -> Keep
      | { Unix.st_kind = S_DIR; _ } -> Dir
      | { Unix.st_kind = S_FIFO | S_CHR | S_BLK | S_SOCK; _ } -> Ignore_silently
      (* This is handled in the Core_scan_function already *)
      | exception Unix.Unix_error (ENOENT, _fun, _info) -> Keep
      (* ignore for now errors. TODO? return a skip? *)
      | exception Unix.Unix_error (_err, _fun, _info) -> Ignore_silently)

(*
   Filter a pre-expanded list of target files, such as a list of files
   obtained with 'git ls-files'.
*)
let filter_paths (ign : Semgrepignore.t) (target_files : Fppath.t list) :
    Fppath_set.t * Out.skipped_target list =
  let (selected_paths : Fppath.t list ref) = ref [] in
  let (skipped : Out.skipped_target list ref) = ref [] in
  let add path = Stack_.push path selected_paths in
  let skip target = Stack_.push target skipped in
  target_files
  |> List.iter (fun fppath ->
         match filter_path ign fppath with
         | Keep -> add fppath
         | Dir ->
             (* shouldn't happen if we work on the output of 'git ls-files *) ()
         | Skip x -> skip x
         | Ignore_silently -> ());
  (Fppath_set.of_list !selected_paths, !skipped)

(* We used to call 'git ls-files' when conf.respect_git_ignore was true,
 * which could potentially speedup things because git may rely on
 * internal data-structures to answer the question instead of walking
 * the filesystem and read the potentially many .gitignore files.
 * However this was not handling .semgrepignore and especially the new
 * ability in osemgrep to negate gitignore decisions in a .semgrepignore,
 * so I think it's simpler to just walk the filesystem whatever the value of
 * conf.respect_git_ignore is. That's what ripgrep does too.
 *
 * python: was called Target.files_from_filesystem ()
 *
 * pre: the scan_root must be a path to a directory
 *)
let walk_skip_and_collect (conf : conf) (ign : Semgrepignore.t)
    (scan_root : Fppath.t) : Fppath.t list * Out.skipped_target list =
  (* Imperative style! walk and collect.
     This is for the sake of readability so let's try to make this as
     readable as possible.
  *)
  let (selected_paths : Fppath.t list ref) = ref [] in
  let (skipped : Out.skipped_target list ref) = ref [] in
  let add path = Stack_.push path selected_paths in
  let skip target = Stack_.push target skipped in

  (* mostly a copy-paste of List_files.list_regular_files() *)
  let rec aux (dir : Fppath.t) =
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
           let fppath : Fppath.t = { fpath; ppath } in
           match filter_path ign fppath with
           | Keep -> add fppath
           | Skip skipped -> skip skipped
           | Dir ->
               (* skipping submodules.
                  TODO? should we add a skip_reason for it? pysemgrep
                  though was using `git ls-files` which implicitely does
                  not even consider submodule files, so those files/dirs
                  were not mentioned in the skip list
               *)
               if
                 conf.respect_gitignore
                 && Git_project.is_git_submodule_root fpath
               then ignore ()
               else
                 (* TODO? if a dir, then add trailing / to ppath
                    and try Git_filter.select() again!
                    (it would detected though anyway in the children of
                    the dir at least, but better to skip the dir ASAP
                 *)
                 aux fppath
           | Ignore_silently -> ())
  in
  aux scan_root;
  (* Let's not worry about file order here until we have to.
     They will be sorted later. *)
  (!selected_paths, !skipped)

(*************************************************************************)
(* Additional Git-specific (or other) expansion of the scanning roots *)
(*************************************************************************)

(*
   Get the list of files being tracked by git. Return a list of paths
   relative to the project root in addition to their system path
   so that we can filter them with semgrepignore.
*)
let git_list_files (file_kinds : Git_wrapper.ls_files_kind list)
    (project_roots : project_roots) : Fppath_set.t option =
  let project = project_roots.project in
  match project.kind with
  | Git_project ->
      Some
        (project_roots.scanning_roots
        |> List.concat_map (fun (sc_root : Fppath.t) ->
               let cwd = Rpath.to_fpath project.path in
               Git_wrapper.ls_files ~cwd ~kinds:file_kinds [ sc_root.fpath ]
               |> List_.map (fun fpath ->
                      let fpath_relative_to_scan_root =
                        match Fpath.relativize ~root:sc_root.fpath fpath with
                        | Some x -> x
                        | None ->
                            failwith
                              (Printf.sprintf "failed to relativize %s to %s"
                                 !!fpath !!(sc_root.fpath))
                      in
                      let ppath =
                        Ppath.append_fpath sc_root.ppath
                          fpath_relative_to_scan_root
                      in
                      ({ fpath; ppath } : Fppath.t)))
        |> Fppath_set.of_list)
  | Gitignore_project
  | Other_project ->
      None

(*
   Get the list of files being tracked by git, return a list of paths
   relative to the project root.

   This doesn't include the "untracked files" reported by 'git status'.
   These untracked files may or may not be desirable. Their fate will be
   determined by the semgrepignore rules separately, along with the gitignored
   files that are not being tracked.

   Returning a set gives us the option to take the union, set difference,
   etc. with other sets of targets.

   We could also provide similar functions for other file tracking systems
   (Mercurial/hg, Subversion/svn, ...)
*)
let git_list_tracked_files (project_roots : project_roots) : Fppath_set.t option
    =
  git_list_files [ Cached ] project_roots

(*
   List all the files that are not being tracked by git except those in
   '.git/'. Return a list of paths relative to the project root.

   This is the complement of git_list_tracked_files (except for '.git/').
*)
let git_list_untracked_files (project_roots : project_roots) :
    Fppath_set.t option =
  git_list_files [ Others ] project_roots

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
  (* Force root relativizes scan roots to project roots.
   * I.e. if the project_root is /repo/src/ and the scanning root is /src/foo
   * it would make the scanning root /foo. So it doesn't make sense to do this
   * with the git remote unless we wanted to make it so git remotes could be
   * further specified (say github.com/semgrep/semgrep.git:/src/foo).
   *)
  let force_root =
    match conf.project_root with
    | Some (Git_remote _)
    | None ->
        None
    | Some (Filesystem proj_root) -> Some (Project.Git_project, proj_root)
  in
  scanning_roots
  |> List_.map (fun scanning_root ->
         let kind, project_root, scanning_root_ppath =
           Git_project.find_any_project_root ?force_root scanning_root
         in
         ( ({ kind; path = Rpath.of_fpath project_root } : Project.t),
           ({ fpath = scanning_root; ppath = scanning_root_ppath } : Fppath.t)
         ))
  (* Using a realpath (physical path) in Project.t ensures we group
     correctly even if the scanning_roots went through different symlink paths.
  *)
  |> Assoc.group_by fst
  |> List.map (fun (project, xs) ->
         { project; scanning_roots = xs |> List_.map snd })

(*************************************************************************)
(* Work on a single project *)
(*************************************************************************)
(*
   We allow multiple scanning roots and they may not all belong to the same
   git project. Most of the logic is done at a project level, though.
*)

let setup_semgrepignore conf (project_roots : project_roots) : Semgrepignore.t =
  let { project = { kind; path = project_root }; scanning_roots = _ } =
    project_roots
  in
  (* filter with .gitignore and .semgrepignore *)
  let exclusion_mechanism =
    match kind with
    | Git_project
    | Gitignore_project ->
        if conf.respect_gitignore then Semgrepignore.Gitignore_and_semgrepignore
        else Semgrepignore.Only_semgrepignore
    | Other_project -> Semgrepignore.Only_semgrepignore
  in
  (* filter also the --include and --exclude from the CLI args
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
  Semgrepignore.create ?include_patterns:conf.include_
    ~cli_patterns:conf.exclude ~builtin_semgrepignore:Semgrep_scan_legacy
    ~exclusion_mechanism
    ~project_root:(Rpath.to_fpath project_root)
    ()

(* Work from a list of  obtained with git *)
let filter_targets conf project_roots (all_files : Fppath.t list) =
  let ign = setup_semgrepignore conf project_roots in
  filter_paths ign all_files

let get_targets_from_filesystem conf (project_roots : project_roots) =
  let ign = setup_semgrepignore conf project_roots in
  List.fold_left
    (fun (selected, skipped) (scan_root : Fppath.t) ->
      (* better: Note that we use Unix.stat below, not Unix.lstat, so
       * osemgrep accepts symlink paths on the command--line;
       * you can do 'osemgrep -e ... ~/symlink-to-proj' or even
       * 'osemgrep -e ... symlink-to-file.py' whereas pysemgrep
       * exits with '"/home/foo/symlink-to-proj" file not found'
       * Note: This may raise Unix.Unix_error.
       * TODO? improve Unix.Unix_error in Find_targets specific exn?
       *)
      let selected2, skipped2 =
        match (Unix.stat !!(scan_root.fpath)).st_kind with
        (* TOPORT? make sure has right permissions (readable) *)
        | S_REG -> ([ scan_root ], [])
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
            ([], [])
      in
      ( Fppath_set.union selected (Fppath_set.of_list selected2),
        List.rev_append skipped2 skipped ))
    (Fppath_set.empty, []) project_roots.scanning_roots

(*
   Target files are identified by following these steps:

   1. A list of folders or files are specified explicitly on the command line.
      These are referred to as "explicit" targets and they should not
      be filtered out even if they match some exclusion patterns.
      This is the input of the 'get_targets' function.
   2. If the project is a git project, use 'git ls-files' or
      equivalent to expand the scanning roots into a list of files.
      This list may include files that would be excluded by the gitignore
      mechanism but are nonetheless being tracked by git (it happens).
   3. The scanning roots from step (1) are expanded using our own
      semgrepignore mechanism. This allows the inclusion of additional
      files that are not under git control because .semgrepignore
      files allows de-exclusion/re-inclusion patterns such as e.g.
      '!*.min.js'.
      Typically, the sets of files produced by (2) and (3) overlap vastly.
   4. Take the union of (2) and (3).
*)
let get_targets_for_project conf (project_roots : project_roots) =
  (* Obtain the list of files from git if possible because it does it
     faster than what we can do by scanning the filesystem: *)
  let git_tracked = git_list_tracked_files project_roots in
  let git_untracked = git_list_untracked_files project_roots in
  let selected_targets, skipped_targets =
    match (git_tracked, git_untracked) with
    | Some tracked, Some untracked ->
        let all_files = Fppath_set.union tracked untracked in
        all_files |> Fppath_set.elements |> filter_targets conf project_roots
    | None, _
    | _, None ->
        get_targets_from_filesystem conf project_roots
  in
  (selected_targets, skipped_targets)

let setup_project_roots conf scanning_roots =
  match conf.project_root with
  | Some (Filesystem _) -> scanning_roots
  | Some (Git_remote { url; checkout_path }) ->
      Logs.debug (fun m ->
          m "Sparse cloning %a into %a" Uri.pp url Fpath.pp checkout_path);
      (match Git_wrapper.sparse_shallow_filtered_checkout url checkout_path with
      | Ok () -> ()
      | Error msg ->
          Logs.err (fun m ->
              m "Error while sparse cloning %a into %a: %s" Uri.pp url Fpath.pp
                checkout_path msg);
          exit 1);
      Git_wrapper.checkout ~cwd:checkout_path ();
      Logs.debug (fun m -> m "Sparse cloning done");

      (* all scanning targets must be in the repo or else this would
         be really weird*)
      scanning_roots
  | None -> scanning_roots

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

let get_targets conf scanning_roots =
  scanning_roots |> setup_project_roots conf
  |> group_scanning_roots_by_project conf
  |> List_.map (get_targets_for_project conf)
  |> List.split
  |> fun (path_set_list, skipped_paths_list) ->
  let path_set =
    List.fold_left Fppath_set.union Fppath_set.empty path_set_list
  in
  let paths = Fppath_set.elements path_set in
  (paths, List.flatten skipped_paths_list)
[@@profiling]

let get_target_fpaths conf scanning_roots =
  let selected, skipped = get_targets conf scanning_roots in
  (selected |> List_.map (fun (x : Fppath.t) -> x.fpath), skipped)
