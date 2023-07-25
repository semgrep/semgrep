module Out = Semgrep_output_v1_t
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

let match_in_dirty_lines (cli_match : Out.cli_match) dirty_lines =
  Array.exists
    (fun (start, end_) ->
      start <= cli_match.start.line && cli_match.start.line <= end_)
    dirty_lines

(** Given a file and some matches, filter the matches out that don't reside
    in lines changed since last commit *)
let filter_clean_lines matches =
  let matches_by_file =
    matches |> Common.group_by (fun (m : Out.cli_match) -> Fpath.v m.path)
  in
  let in_git, not_in_git =
    matches_by_file
    |> List.partition (fun (f, _) ->
           let parent = Fpath.parent f in
           Git_wrapper.is_git_repo parent)
  in
  let in_git_matches =
    in_git
    |> List.concat_map (fun (f, matches) ->
           match Git_wrapper.dirty_lines_of_file f with
           | None -> matches
           | Some dirty_lines ->
               List.filter (fun m -> match_in_dirty_lines m dirty_lines) matches)
  in
  let not_in_git_matches = not_in_git |> List.concat_map snd in
  in_git_matches @ not_in_git_matches

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

let of_matches ?(only_git_dirty = true) (result : Core_runner.result) =
  let matches = result.core.matches in
  let hrules = result.hrules in
  let env = { Cli_json_output.hrules } in
  (* Match up the rules with the matches so we can get fixes, rule ids, messages *)
  let matches =
    matches
    |> Common.map (Cli_json_output.cli_match_of_core_match env)
    |> Common.exclude (fun m ->
           let to_ignore, _errs = Nosemgrep.rule_match_nosem ~strict:false m in
           to_ignore)
  in
  (* Filter dirty lines *)
  if only_git_dirty then filter_clean_lines matches else matches
