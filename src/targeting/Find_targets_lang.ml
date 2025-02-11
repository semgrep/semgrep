(* This function should be used mostly in testing code to quickly get a list
 * of targets for a certain language. For real semgrep targeting, use the
 * Core_targeting module.
 *)
let get_target_fpaths (caps : < Cap.readdir ; .. >) (root : Fpath.t)
    (lang : Lang.t) : Fpath.t list =
  let conf =
    {
      Find_targets.default_conf with
      (* otherwise we can't run for example -dump_name_asts tests/interfile/.. *)
      force_project_root = Some (Filesystem (Rfpath.of_fpath_exn root));
    }
  in
  (* coupling: similar to what we do Scan_subcommand.run_scan_conf()
   * old: Find_targets_old.files_of_dirs_or_files (Some lang) [ root ]
   * TODO? at least Logs the errors and skipped?
   *)
  let files, _errors, _skipped =
    Find_targets.get_target_fpaths caps conf [ Scanning_root.of_fpath root ]
  in
  (* filter out files that are not relevant to the language here, because
   * `Find_targets` fetches _all_ the possibly relevant files it can.
   * Without this, we end up trying to parse a bunch of files, like Markdown,
   * which isn't helpful.
   * coupling: similar to what we do in Core_targeting.split_jobs_by_language()
   *)
  files
  |> List.filter
       (Filter_target.filter_target_for_analyzer (Analyzer.of_lang lang))
