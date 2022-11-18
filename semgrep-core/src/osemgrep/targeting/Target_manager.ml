(*
   Translated from target_manager.py
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
*)
type path = string

type _t = {
  todo : unit;
      (* TODO
          target_strings: Sequence[str]
          includes: Sequence[str] = Factory(list)
          excludes: Sequence[str] = Factory(list)
          max_target_bytes: int = -1
          respect_git_ignore: bool = False
          baseline_handler: Optional[BaselineHandler] = None
          allow_unknown_extensions: bool = False
          file_ignore: Optional[FileIgnore] = None
          lockfile_scan_info: Dict[str, int] = {}
          ignore_log: FileTargetingLog = Factory(FileTargetingLog, takes_self=True)
          targets: Sequence[Target] = field(init=False)

          _filtered_targets: Dict[Language, FilteredFiles] = field(factory=dict)
      *)
}

let get_targets ~includes ~excludes ~max_target_bytes ~respect_git_ignore
    target_roots =
  Find_target.select_global_targets ~includes ~excludes ~max_target_bytes
    ~respect_git_ignore target_roots
