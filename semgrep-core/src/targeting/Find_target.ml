(*
   Find and filter targets.

   TODO: note that some of the functions below are also present in
   semgrep-python so we might want to move all file-targeting in one
   place.
*)

module Resp = Output_from_core_t

let sort_by_decreasing_size paths =
  paths
  |> Common.map (fun path -> (path, Common2.filesize path))
  |> List.sort (fun (_, (a : int)) (_, b) -> compare b a)
  |> Common.map fst

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
  let paths, skipped1 = Skip_target.exclude_files_in_skip_lists paths in
  let paths, skipped2 =
    match opt_lang with
    | None -> (paths, [])
    | Some lang -> Guess_lang.inspect_files lang paths
  in
  let paths, skipped3 = Skip_target.exclude_big_files paths in
  let paths, skipped4 = Skip_target.exclude_minified_files paths in
  let skipped = Common.flatten [ skipped1; skipped2; skipped3; skipped4 ] in
  let paths = explicit_targets @ paths in
  let sorted_paths =
    if sort_by_decr_size then sort_by_decreasing_size paths
    else List.sort String.compare paths
  in
  let sorted_skipped =
    List.sort
      (fun (a : Resp.skipped_target) b -> String.compare a.path b.path)
      skipped
  in
  (sorted_paths, sorted_skipped)
