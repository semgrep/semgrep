(*
   Find and filter targets.
*)

open Common
module Resp = Semgrep_core_response_t

let exclude_files_in_skip_lists roots =
  let paths, skipped_paths =
    Skip_code.filter_files_if_skip_list ~root:roots roots
  in
  let skipped =
    skipped_paths
    |> Common.map (fun path ->
           {
             Resp.path;
             reason = Excluded_by_config;
             details = "excluded by 'skip list' file";
             skipped_rule = None;
           })
  in
  (paths, skipped)

(*
   Some source files are really huge (> 20 MB) and they cause
   some annoying 'out of memory' crash that sometimes even the use
   of mem_limit_mb above does not solve.

   We could configure the size limit based on a per-language basis if we
   know that a language parser can handle larger files.
*)
let filter_by_size _lang paths =
  let max_bytes = !Flag_semgrep.max_target_bytes in
  paths
  |> Common.partition_result (fun path ->
         let size = Common2.filesize path in
         if size > max_bytes then
           Error
             {
               Resp.path;
               reason = Too_big;
               details =
                 spf "target file size exceeds %i bytes at %i bytes" max_bytes
                   size;
               skipped_rule = None;
             }
         else Ok path)

let files_of_dirs_or_files ?(keep_root_files = true) lang roots =
  let explicit_targets, paths =
    if keep_root_files then
      roots
      |> List.partition (fun path ->
             Sys.file_exists path && not (Sys.is_directory path))
    else (roots, [])
  in
  let paths = Common.files_of_dir_or_files_no_vcs_nofilter paths in
  let paths, skipped1 = exclude_files_in_skip_lists paths in
  let paths, skipped2 = Guess_lang.inspect_files lang paths in
  let paths, skipped3 = filter_by_size lang paths in
  let skipped = Common.flatten [ skipped1; skipped2; skipped3 ] in
  let sorted_paths = List.sort String.compare (explicit_targets @ paths) in
  let sorted_skipped =
    List.sort
      (fun (a : Resp.skipped_target) b -> String.compare a.path b.path)
      skipped
  in
  (sorted_paths, sorted_skipped)
