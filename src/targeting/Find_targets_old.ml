open Fpath_.Operators
module Out = Semgrep_output_v1_t
module Resp = Semgrep_output_v1_t
module Log = Log_targeting.Log

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* Deprecated: use Find_targets.ml instead. *)

(*************************************************************************)
(* Sorting *)
(*************************************************************************)

(* similar to Run_semgrep.sort_targets_by_decreasing_size, could factorize *)
let sort_files_by_decreasing_size files =
  files
  |> List_.map (fun file -> (file, UFile.filesize file))
  |> List.sort (fun (_, (a : int)) (_, b) -> compare b a)
  |> List_.map fst

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
  let paths, skipped1 = Skip_target.exclude_inaccessible_files paths in
  let paths, skipped2 =
    match opt_lang with
    | None -> (paths, [])
    | Some lang -> Guess_lang.inspect_files lang paths
  in
  let paths, skipped3 =
    Skip_target.exclude_big_files !Flag_semgrep.max_target_bytes paths
  in
  let paths, skipped4 = Skip_target.exclude_minified_files paths in
  let skipped = List_.flatten [ skipped1; skipped2; skipped3; skipped4 ] in
  let sorted_paths =
    if sort_by_decr_size then sort_files_by_decreasing_size paths else paths
  in
  let sorted_skipped =
    List.sort
      (fun (a : Out.skipped_target) b -> Fpath.compare a.path b.path)
      skipped
  in
  (sorted_paths, sorted_skipped)
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
  let paths = UFile.files_of_dirs_or_files_no_vcs_nofilter paths in

  let paths, skipped = global_filter ~opt_lang ~sort_by_decr_size paths in
  let paths = explicit_targets @ paths in
  let sorted_paths =
    if sort_by_decr_size then sort_files_by_decreasing_size paths
    else List.sort Fpath.compare paths
  in
  let sorted_skipped =
    List.sort
      (fun (a : Out.skipped_target) b -> Fpath.compare a.path b.path)
      skipped
  in
  (sorted_paths, sorted_skipped)
