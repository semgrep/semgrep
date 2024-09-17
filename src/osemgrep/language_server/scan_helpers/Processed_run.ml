module OutJ = Semgrep_output_v1_t
(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* This module postprocess semgrep run results given rules and files
 * scanned, populating fixes, and messages. It also filters out matches
 * depending on 'git status'.
 *)

(*************************************************************************)
(* Types *)
(*************************************************************************)

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

(** Checks if the range of [cli_match] overlaps with [dirty_lines] *)
let match_in_dirty_lines (cli_match : OutJ.cli_match) dirty_lines =
  Array.exists
    (fun (start, end_) ->
      start <= cli_match.end_.line && cli_match.start.line <= end_)
    dirty_lines

(** Given a file and some matches, filter the matches out that don't reside
    in lines changed since last commit *)
let filter_clean_lines git_ref matches =
  let matches_by_file =
    matches |> Assoc.group_by (fun (m : OutJ.cli_match) -> m.path)
  in
  let in_git, not_in_git =
    matches_by_file
    |> List.partition (fun (file_path, _) ->
           (* If the git_ref is set here, we might be comparing a tracked file
              with some sort of temporary file *)
           Option.is_some git_ref || Git_wrapper.is_tracked_by_git file_path)
  in
  let git_ref = Option.value ~default:"HEAD" git_ref in
  let in_git_matches =
    in_git
    |> List.concat_map (fun (f, matches) ->
           match Git_wrapper.dirty_lines_of_file ~git_ref f with
           | None -> matches
           | Some dirty_lines ->
               List.filter (fun m -> match_in_dirty_lines m dirty_lines) matches)
  in
  let not_in_git_matches = not_in_git |> List.concat_map snd in
  in_git_matches @ not_in_git_matches

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

let of_matches ?(skipped_fingerprints = []) ?(only_git_dirty = true) ?git_ref
    (result : Core_runner.result) =
  let result = Output.preprocess_result ~fixed_lines:false result in
  (* Match the rules with the matches so we can get fixes/rule-ids/messages *)
  let matches =
    result.results
    |> List.filter (fun (m : OutJ.cli_match) ->
           not (List.mem m.extra.fingerprint skipped_fingerprints))
  in
  (* Filter dirty lines *)
  if only_git_dirty then filter_clean_lines git_ref matches else matches
