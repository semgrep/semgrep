(* Martin Jambon, Yoann Padioleau
 *
 * Copyright (C) 2023-2024 Semgrep Inc.
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
module Out = Semgrep_output_v1_t
module Log = Log_targeting.Log

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

   TODO:
    - optimize, reduce the number of filesystem lookup? or memoize them?
      there are a few places where we stat for a file
    - add an option to select all git-tracked files regardless of
      gitignore or semgrepignore exclusions (will be needed for Secrets)
      and have the exclusions apply only to the files that aren't tracked.
*)

(*************************************************************************)
(* Types *)
(*************************************************************************)

type project_root =
  | Filesystem of Rfpath.t
  (* for Semgrep query console *)
  | Git_remote of git_remote

and git_remote = { url : Uri.t } [@@deriving show]

module Fppath_set = Set.Make (Fppath)

(* Yet another file path related type ...

   This module is a bit fragile as it assumes that target file paths found in
   the file system have the same form as those passed on the command line.
   It won't work with unnormalized paths such as 'foo/../bar.js' that will
   likely be rewritten into 'bar.js'. See:

     $ git ls-files libs/../README.md
     README.md

   This results in 'README.md' being treated as non-explicit target file.

   TODO: use pairs (project, ppath) instead as keys? If we use a dedicated
   record for targets, we can extract the pair (project, ppath):

     type target = {
       project: Project.t; (* provides normalized project root *)
       path: Fppath.t; (* provides (normalized) ppath *)
     }

   If we go this path, we could also add a field 'is_explicit: bool' to the
   target type.
*)
module Explicit_targets = struct
  type t = {
    tbl : (Fpath.t, unit) Hashtbl.t;
        [@printer fun fmt _tbl -> fprintf fmt "<hashtbl>"]
    (* Elements in their original order *)
    list : Fpath.t list;
  }
  [@@deriving show]

  let empty = { tbl = Hashtbl.create 0; list = [] }

  let of_list paths =
    let tbl = Hashtbl.create (2 * List.length paths) in
    List.iter (fun path -> Hashtbl.replace tbl path ()) paths;
    { tbl; list = paths }

  let to_list x = x.list

  (* Fast O(1) operation *)
  let mem x path = Hashtbl.mem x.tbl path
end

type conf = {
  (* global exclude list, passed via semgrep '--exclude'.
   * TODO? use Glob.Pattern.t instead? same for include_
   *)
  exclude : string list;
  (* !!! '--include' is very different from '--exclude' !!!
      The include filter is applied after after gitignore and
      semgrepignore filters. It doesn't override them.

     This field holds a list of patterns passed via 'semgrep --include'
     [!] include_ = None is the opposite of Some [].
     If a list of include patterns is specified, a path must match
     at least of the patterns to be selected.
     ('--require' might make a better flag name, but both grep and ripgrep
      use the '--exclude' and '--include' names).
  *)
  include_ : string list option;
  max_target_bytes : int;
  (* Whether or not follow what is specified in the .gitignore
   * The .semgrepignore are always respected.
   *)
  respect_gitignore : bool;
  always_select_explicit_targets : bool;
  explicit_targets : Explicit_targets.t;
  (* osemgrep-only: option
     (see Git_project.find_any_project_root and the force_root parameter) *)
  force_project_root : project_root option;
  (* osemgrep-only option, exclude scanning minified files, default false *)
  exclude_minified_files : bool;
  (* TODO? remove it? This is now done in Diff_scan.ml instead? *)
  baseline_commit : string option;
  diff_depth : int;
}
[@@deriving show]

(*************************************************************************)
(* Defaults *)
(*************************************************************************)

let default_conf : conf =
  {
    force_project_root = None;
    exclude = [];
    include_ = None;
    (* Must be kept in sync w/ pysemgrep.
       coupling: cli/src/semgrep/constants.py DEFAULT_MAX_TARGET_SIZE
    *)
    max_target_bytes = 1000000;
    respect_gitignore = true;
    always_select_explicit_targets = false;
    explicit_targets = Explicit_targets.empty;
    exclude_minified_files = false;
    baseline_commit = None;
    diff_depth = 2;
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
(* Filtering *)
(*************************************************************************)

type filter_result =
  | Keep (* select this target file *)
  | Dir (* the path is a directory to scan recursively *)
  | Skip of Out.skipped_target (* ignore this file and report it *)
  | Ignore_silently (* ignore and don't report this file *)

let ignore_path selection_events fpath =
  Log.debug (fun m ->
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

let apply_include_filter status selection_events include_filter ppath =
  match status with
  | Gitignore.Ignored -> (status, selection_events)
  | Gitignore.Not_ignored -> (
      match include_filter with
      | None -> (status, selection_events)
      | Some include_filter -> Include_filter.select include_filter ppath)

(* Note that include_filter applies only to the paths of regular files. They're
 * applied last, after the exclude/gitignore/semgrepignore filters.
 *)
let filter_path (ign : Gitignore.filter)
    (include_filter : Include_filter.t option) (fppath : Fppath.t) :
    filter_result =
  let { fpath; ppath } : Fppath.t = fppath in
  let status, selection_events = Gitignore_filter.select ign ppath in
  match status with
  | Ignored -> ignore_path selection_events fpath
  | Not_ignored -> (
      (* TODO: check read permission too? *)
      match (Unix.lstat !!fpath).st_kind with
      (* skipping symlinks *)
      | S_LNK -> Ignore_silently
      | S_REG -> (
          let status, selection_events =
            apply_include_filter status selection_events include_filter ppath
          in
          match status with
          | Ignored -> ignore_path selection_events fpath
          | Not_ignored -> Keep)
      | S_DIR -> Dir
      | S_FIFO
      | S_CHR
      | S_BLK
      | S_SOCK ->
          Ignore_silently
      (* We need to filter those paths ASAP otherwise we can get some exn later
       * when trying to process targets that actually do not exist.
       *)
      | exception Unix.Unix_error (_err, _fun, _info) -> Ignore_silently)

(*
   Filter a pre-expanded list of target files, such as a list of files
   obtained with 'git ls-files'. A strong postcondition is that the
   paths returned must correspond to existing regular files!
*)
let filter_paths
    ((ign, include_filter) : Gitignore.filter * Include_filter.t option)
    (target_files : Fppath.t list) : Fppath_set.t * Out.skipped_target list =
  let (selected_paths : Fppath.t list ref) = ref [] in
  let (skipped : Out.skipped_target list ref) = ref [] in
  let add path = Stack_.push path selected_paths in
  let skip target = Stack_.push target skipped in
  target_files
  |> List.iter (fun fppath ->
         match filter_path ign include_filter fppath with
         | Keep -> add fppath
         (* shouldn't happen if we work on the output of 'git ls-files *)
         | Dir -> ()
         | Skip x -> skip x
         | Ignore_silently -> ());
  (Fppath_set.of_list !selected_paths, !skipped)

let filter_size_and_minified max_target_bytes exclude_minified_files paths =
  let selected_fppaths, skipped_size =
    Result_.partition
      (fun (fppath : Fppath.t) ->
        Result.map
          (fun _ -> fppath)
          (Skip_target.is_big max_target_bytes fppath.fpath))
      paths
  in
  let selected_fppaths, skipped_minified =
    if exclude_minified_files then
      Result_.partition
        (fun (fppath : Fppath.t) ->
          Result.map (fun _ -> fppath) (Skip_target.is_minified fppath.fpath))
        selected_fppaths
    else (selected_fppaths, [])
  in
  Log.debug (fun m -> m "skipped_size: %d" (List.length skipped_size));
  Log.debug (fun m -> m "skipped_minified: %d" (List.length skipped_minified));
  (selected_fppaths, skipped_size @ skipped_minified)

(*************************************************************************)
(* Finding by walking *)
(*************************************************************************)

(* We used to call 'git ls-files' when conf.respect_gitignore was true,
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
let walk_skip_and_collect (ign : Gitignore.filter)
    (include_filter : Include_filter.t option) (scan_root : Fppath.t) :
    Fppath.t list * Out.skipped_target list =
  Log.info (fun m ->
      m "scanning file system starting from root %s" (Fppath.show scan_root));
  (* Imperative style! walk and collect.
     This is for the sake of readability so let's try to make this as
     readable as possible.
  *)
  let (selected_paths : Fppath.t list ref) = ref [] in
  let (skipped : Out.skipped_target list ref) = ref [] in

  (* TODO: factorize code with filter_paths? *)
  let add path = Stack_.push path selected_paths in
  let skip target = Stack_.push target skipped in

  (* mostly a copy-paste of List_files.list_regular_files() *)
  let rec aux (dir : Fppath.t) =
    Log.debug (fun m ->
        m "listing dir %s (ppath = %s)" !!(dir.fpath)
          (Ppath.to_string_for_tests dir.ppath));
    (* TODO? should we sort them first? *)
    let entries = List_files.read_dir_entries dir.fpath in
    (* TODO: factorize code with filter_paths? *)
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
           match filter_path ign include_filter fppath with
           | Keep -> add fppath
           | Skip skipped -> skip skipped
           | Dir -> aux fppath
           | Ignore_silently -> ())
  in
  aux scan_root;
  (* Let's not worry about file order here until we have to.
     They will be sorted later. *)
  (!selected_paths, !skipped)

(*************************************************************************)
(* Finding by using git *)
(*************************************************************************)

(*
   Get the list of files being tracked by git. Return a list of paths
   relative to the project root in addition to their system path
   so that we can filter them with semgrepignore.

   exclude_standard is the --exclude-standard flag to 'git ls-files'
   and requests filtering based on gitignore rules. We don't want it when
   obtaining the list of tracked files because some files can be tracked
   despite being excluded by gitignore.
*)
let git_list_files ~exclude_standard
    (file_kinds : Git_wrapper.ls_files_kind list)
    (project_roots : Project.roots) : Fppath_set.t option =
  Log.debug (fun m ->
      m "Find_targets.git_list_files for project %s"
        (Project.show project_roots.project));
  let project = project_roots.project in
  (* TODO: we should not call git_list_files when the project
   * is not a Git_project. We should assert it and not return
   * an option type but an Fppath_set.t instead.
   *)
  match project.kind with
  | Git_project ->
      let cwd = Fpath.v (Sys.getcwd ()) in
      Some
        (project_roots.scanning_roots
        |> List.concat_map (fun (sc_root : Fppath.t) ->
               Log.info (fun m ->
                   m "List git files for scanning root %S" !!(sc_root.fpath));
               let project_root = Rfpath.to_rpath project.root in
               (* The path prefix we want for all the target file paths
                  that we return *)
               let orig_scanning_root_path = sc_root.fpath in
               (* Best effort to get a relative scanning root path
                  (will fail in file systems with multiple roots) *)
               let rel_scanning_root_path_or_absolute =
                 if Fpath.is_rel orig_scanning_root_path then
                   orig_scanning_root_path
                 else
                   match Fpath.relativize ~root:cwd orig_scanning_root_path with
                   | Some rel_scanning_root -> rel_scanning_root
                   | None ->
                       (* absolute, on another volume than cwd *)
                       orig_scanning_root_path
               in
               (* We can't just cd into the scanning root to obtain paths
                  relative to it because the scanning root may be a regular
                  file. It could also be the root of the file system, so we
                  also can't cd into its parent.
                  This is why we stay in the same cwd and only later convert
                  the resulting paths to be relative to the scanning root. *)
               Git_wrapper.ls_files_relative ~exclude_standard ~kinds:file_kinds
                 ~project_root
                 [ orig_scanning_root_path ]
               |> List_.map (fun target_relative_to_cwd_or_absolute ->
                      (* Invariant: the target path is a descendant of the scanning
                         root path (possibly the scanning root path itself) *)

                      (* Obtain a path whose prefix is the original scanning root
                         if possible.
                         If the scanning root is './proj/lib',
                         then we want a result target path to be
                         './proj/lib/../hello.c', not the equivalent
                         'proj/hello.c'.
                         The only exception is if the scanning root is '.',
                         in which case we don't produce './foo' but 'foo'.
                      *)
                      let target_fpath =
                        match
                          Fpath.relativize
                            ~root:rel_scanning_root_path_or_absolute
                            target_relative_to_cwd_or_absolute
                        with
                        | Some target_relative_to_scan_root ->
                            Fpath_.append_no_dot orig_scanning_root_path
                              target_relative_to_scan_root
                        | None -> target_relative_to_cwd_or_absolute
                      in
                      (* Obtain a path relative to the project root *)
                      let target_ppath =
                        match
                          Fpath.relativize
                            ~root:(Rpath.to_fpath project_root)
                            (cwd // target_relative_to_cwd_or_absolute)
                        with
                        | None ->
                            (* TODO: return an Error instead and let the
                             * caller decide instead of assert false
                             *)
                            (* nosemgrep: no-logs-in-library *)
                            Logs.err (fun m ->
                                m
                                  "Internal error: cannot obtain path relative \
                                   to project root from project_root=%s, \
                                   cwd=%S, path_relative_to_cwd=%S"
                                  !!(Rpath.to_fpath project_root)
                                  !!cwd
                                  !!target_relative_to_cwd_or_absolute);
                            assert false
                        | Some fpath_relative_to_project_root ->
                            Ppath.of_relative_fpath
                              fpath_relative_to_project_root
                      in
                      ({ fpath = target_fpath; ppath = target_ppath }
                        : Fppath.t)))
        |> Fppath_set.of_list)
  | _else_ -> None

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
let git_list_tracked_files (project_roots : Project.roots) : Fppath_set.t option
    =
  git_list_files ~exclude_standard:false [ Cached ] project_roots

(*
   List all the files that are not being tracked by git except those in
   '.git/'. Return a list of paths relative to the project root.

   This is the complement of git_list_tracked_files (except for '.git/').
*)
let git_list_untracked_files (project_roots : Project.roots) :
    Fppath_set.t option =
  git_list_files ~exclude_standard:true [ Others ] project_roots

(*************************************************************************)
(* Grouping *)
(*************************************************************************)

let scanning_root_by_project (force_root : Project.t option)
    (scanning_root : Scanning_root.t) : Project.t * Fppath.t =
  let scanning_root_fpath = Scanning_root.to_fpath scanning_root in
  let kind, scanning_root_info =
    Project.find_any_project_root ?force_root scanning_root_fpath
  in
  let project : Project.t = { kind; root = scanning_root_info.project_root } in
  let path : Fppath.t =
    { fpath = scanning_root_fpath; ppath = scanning_root_info.inproject_path }
  in
  (project, path)

(*
   Identify the project root for each scanning root and group them
   by project root. If the project_root is specified, then we use that.

   This is important to avoid reading the gitignore and semgrepignore files
   twice when multiple scanning roots that belong to the same project.

   TODO? move in paths/Project.ml?
*)
let group_scanning_roots_by_project (conf : conf)
    (scanning_roots : Scanning_root.t list) : Project.roots list =
  (* Force root relativizes scan roots to project roots.
     I.e. if the project_root is /repo/src/ and the scanning root is /src/foo
     it would make the scanning root /foo. So it doesn't make sense to
     combine this with the git remote unless we wanted to make it so git
     remotes could be further specified (say
     github.com/semgrep/semgrep.git:/src/foo).

     TODO: revise the above. 'force_root' is the project root.
  *)
  Log.debug (fun m ->
      m "group_scanning_roots_by_project %s"
        (Logs_.list Scanning_root.to_string scanning_roots));
  let force_root : Project.t option =
    match conf.force_project_root with
    | Some (Filesystem proj_root) ->
        (* This is when --project-root is specified on the command line.
           It doesn't use 'git ls-files' to list files. This is required
           for some tests to pass within our semgrep repo but it's not clear
           why it's like this.
           TODO: make tests work without requiring --project-root? *)
        Some Project.{ kind = Project.Gitignore_project; root = proj_root }
    | Some (Git_remote _)
    | None ->
        (* Usual case when scanning the local file system *)
        None
  in
  scanning_roots
  |> List_.map (scanning_root_by_project force_root)
  (* Using a realpath (physical path) in Project.t ensures we group
     correctly even if the scanning_roots went through different symlink paths.
  *)
  |> Assoc.group_assoc_bykey_eff
  |> List_.map (fun (project, scanning_roots) ->
         Project.{ project; scanning_roots })

(*************************************************************************)
(* Work on a single project *)
(*************************************************************************)
(*
   We allow multiple scanning roots and they may not all belong to the same
   git project. Most of the logic is done at a project level, though.
*)

let setup_path_filters conf (project_roots : Project.roots) :
    Gitignore.filter * Include_filter.t option =
  let Project.{ project = { kind; root = project_root }; scanning_roots = _ } =
    project_roots
  in
  (* filter with .gitignore and .semgrepignore *)
  let exclusion_mechanism =
    match kind with
    | Git_project
    | Gitignore_project ->
        if conf.respect_gitignore then Semgrepignore.Gitignore_and_semgrepignore
        else Semgrepignore.Only_semgrepignore
    | Mercurial_project
    | Subversion_project
    | Darcs_project
    | Other_project ->
        Semgrepignore.Only_semgrepignore
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
  let semgrepignore_filter =
    Semgrepignore.create ~cli_patterns:conf.exclude
      ~builtin_semgrepignore:Semgrep_scan_legacy ~exclusion_mechanism
      ~project_root:(Rfpath.to_fpath project_root)
      ()
  in
  let include_filter =
    Option.map
      (Include_filter.create ~project_root:(Rfpath.to_fpath project_root))
      conf.include_
  in
  (semgrepignore_filter, include_filter)

(* Work from a list of target paths obtained with git *)
let filter_targets conf project_roots (all_files : Fppath.t list) =
  let ign = setup_path_filters conf project_roots in
  filter_paths ign all_files

let get_targets_from_filesystem conf (project_roots : Project.roots) =
  let ign, include_filter = setup_path_filters conf project_roots in
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
        | S_DIR -> walk_skip_and_collect ign include_filter scan_root
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
let get_targets_for_project conf (project_roots : Project.roots) =
  Log.debug (fun m -> m "Find_target.get_targets_for_project");
  (* Obtain the list of files from git if possible because it does it
     faster than what we can do by scanning the filesystem: *)
  let git_tracked = git_list_tracked_files project_roots in
  let git_untracked = git_list_untracked_files project_roots in
  let selected_targets, skipped_targets =
    match (git_tracked, git_untracked) with
    | Some tracked, Some untracked ->
        Log.debug (fun m ->
            m "target file candidates from git: tracked: %i, untracked: %i"
              (Fppath_set.cardinal tracked)
              (Fppath_set.cardinal untracked));
        let all_files = Fppath_set.union tracked untracked in
        all_files |> Fppath_set.elements |> filter_targets conf project_roots
    | None, _
    | _, None ->
        get_targets_from_filesystem conf project_roots
  in
  (selected_targets, skipped_targets)

(* for semgrep query console *)
let clone_if_remote_project_root conf =
  match conf.force_project_root with
  | Some (Git_remote { url }) ->
      let cwd = Fpath.v (Unix.getcwd ()) in
      Log.info (fun m ->
          m "Sparse cloning %a into CWD: %a" Uri.pp url Fpath.pp cwd);
      (match Git_wrapper.sparse_shallow_filtered_checkout url (Fpath.v ".") with
      | Ok () -> ()
      | Error msg ->
          failwith
            (spf "Error while sparse cloning %s into %s: %s" (Uri.to_string url)
               (Fpath.to_string cwd) msg));
      Git_wrapper.checkout ();
      Log.info (fun m -> m "Sparse cloning done")
  | Some (Filesystem _)
  | None ->
      ()

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

let get_targets conf scanning_roots =
  clone_if_remote_project_root conf;
  scanning_roots
  |> group_scanning_roots_by_project conf
  |> List_.map (get_targets_for_project conf)
  |> List.split
  |> fun (path_set_list, skipped_paths_list) ->
  let paths, skipped_size_minified =
    let path_set =
      List.fold_left Fppath_set.union Fppath_set.empty path_set_list
    in
    Fppath_set.elements path_set
    |> filter_size_and_minified conf.max_target_bytes
         conf.exclude_minified_files
  in
  let sorted_skipped_targets =
    let skipped_paths_list =
      List_.flatten skipped_paths_list @ skipped_size_minified
    in
    skipped_paths_list
    |> List.sort (fun (a : Out.skipped_target) (b : Out.skipped_target) ->
           Fpath.compare a.path b.path)
  in
  (paths, sorted_skipped_targets)
[@@profiling]

let get_target_fpaths conf scanning_roots =
  let selected, skipped = get_targets conf scanning_roots in
  let selected_fpaths = selected |> List_.map (fun (x : Fppath.t) -> x.fpath) in
  (selected_fpaths, skipped)
