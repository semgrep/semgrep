open Common
open File.Operators
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

module Fpath_set = Set.Make (Fpath)

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
  diff_depth : int;
  (* TODO: use *)
  scan_unknown_extensions : bool;
  (* osemgrep-only: option (see Git_project.ml and the force_root parameter) *)
  project_root : Fpath.t option;
}
[@@deriving show]

(* For gitignore filtering, we need to operate on Ppath (see
 * the signature of Gitignore_filter.select()), but when semgrep
 * displays findings or errors, we want filenames derived from
 * the scanning roots, not the root of the project. This is why we need to
 * keep both the fpath and ppath for each target file as we walked
 * down the filesystem hierarchy.
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
    (scan_root : fppath) : Fpath.t list * Out.skipped_target list =
  (* Imperative style! walk and collect.
     This is for the sake of readability so let's try to make this as
     readable as possible.
  *)
  let (selected_paths : Fpath.t list ref) = ref [] in
  let (skipped : Out.skipped_target list ref) = ref [] in
  let add path = push path selected_paths in
  let skip target = push target skipped in

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
           (* skip hidden files (this includes big directories like .git/)
              TODO? maybe add a setting in conf? -> no, this is a job for
              semgrepignore.
              TODO: why are we doing this? It doesn't seem like pysemgrep
              is doing this.
           *)
           if name =~ "^\\." then
             skip
               {
                 Out.path = fpath;
                 reason = Out.Dotfile;
                 details = None;
                 rule_id = None;
               }
           else
             let status, selection_events = Semgrepignore.select ign ppath in
             match status with
             | Ignored ->
                 Logs.debug (fun m ->
                     m "Ignoring path %s:\n%s" !!fpath
                       (Gitignore.show_selection_events selection_events));
                 let reason = get_reason_for_exclusion selection_events in
                 skip
                   {
                     Out.path = fpath;
                     reason;
                     details =
                       Some
                         "excluded by --include/--exclude, gitignore, or \
                          semgrepignore";
                     rule_id = None;
                   }
             | Not_ignored -> (
                 (* TODO: check read permission? *)
                 match Unix.lstat !!fpath with
                 (* skipping symlinks *)
                 | { Unix.st_kind = S_LNK; _ } -> ()
                 | { Unix.st_kind = S_REG; _ } -> add fpath
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
                       (* TODO? if a dir, then add trailing / to ppath
                        * and try Git_filter.select() again!
                        * (it would detected though anyway in the children of
                        * the dir at least, but better to skip the dir ASAP
                        *)
                     else aux { fpath; ppath }
                 | { Unix.st_kind = S_FIFO | S_CHR | S_BLK | S_SOCK; _ } -> ()
                 (* ignore for now errors. TODO? return a skip? *)
                 | exception Unix.Unix_error (_err, _fun, _info) -> ()))
  in
  aux scan_root;
  (* Let's not worry about file order here until we have to.
     They will be sorted later. *)
  (!selected_paths, !skipped)

(*************************************************************************)
(* Additional Git-specific (or other) expansion of the scanning roots *)
(*************************************************************************)

(*
   Get the list of files being tracked by git.

   This doesn't include the "untracked files" reported by 'git status'.
   These untracked files may or may not be desirable. Their fate will be
   determined by the semgrepignore rules separately, along with the gitignored
   files that are not being tracked.

   Returning a set gives us the option to take the union, set difference,
   etc. with other sets of targets.

   We could also provide similar functions for other file tracking systems
   (Mercurial/hg, Subversion/svn, ...)
*)
let _git_ls_files (project_roots : project_roots) : Fpath_set.t =
  match project_roots.project.kind with
  | Git_project ->
      let (Realpath project_path) = project_roots.project.path in
      project_roots.scanning_roots
      |> Common.map (fun (x : fppath) -> x.fpath)
      |> Git_wrapper.ls_files ~cwd:project_path
      |> Fpath_set.of_list
  | Other_project -> Fpath_set.empty

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
         ( ({ kind; path = Realpath.of_fpath project_root } : Project.t),
           { fpath = scanning_root; ppath = scanning_root_ppath } ))
  (* using a Realpath in Project.t ensures we group correctly even
   * if the scanning_roots went through different symlink paths
   *)
  |> Common.group_by fst
  |> Common.map (fun (project, xs) ->
         { project; scanning_roots = xs |> Common.map snd })

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

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
let get_targets conf scanning_roots =
  scanning_roots
  |> group_scanning_roots_by_project conf
  |> List.concat_map
       (fun { project = { kind; path = project_root }; scanning_roots } ->
         (* filter with .gitignore and .semgrepignore *)
         let exclusion_mechanism =
           match kind with
           | Project.Git_project ->
               if conf.respect_git_ignore then
                 Semgrepignore.Gitignore_and_semgrepignore
               else Semgrepignore.Only_semgrepignore
           | Project.Other_project -> Semgrepignore.Only_semgrepignore
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
         let ign =
           Semgrepignore.create ?include_patterns:conf.include_
             ~cli_patterns:conf.exclude
             ~builtin_semgrepignore:Semgrep_scan_legacy ~exclusion_mechanism
             ~project_root:(Realpath.to_fpath project_root)
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
