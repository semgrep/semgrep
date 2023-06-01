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

type t = Semgrep_output_v1_t.core_match * Rule.rule

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

(** Given a file and some matches, filter the matches out that don't reside
    in lines changed since last commit *)
let filter_dirty_lines files matches =
  let dirty_files = Hashtbl.create 10 in
  files
  |> List.iter (fun f ->
         let dirty_lines = Git_wrapper.dirty_lines_of_file f in
         Hashtbl.add dirty_files f dirty_lines);
  matches
  |> List.filter (fun ((m, _) : t) ->
         let dirty_lines =
           Hashtbl.find_opt dirty_files (Fpath.v m.location.path)
         in
         let line = m.location.start.line in
         match dirty_lines with
         | None -> false
         | Some [||] -> true (* Untracked files *)
         | Some dirty_lines ->
             Array.exists
               (fun (start, end_) -> start <= line && line <= end_)
               dirty_lines)

(** Get the first and previous line of a match *)
let get_match_lines (loc : Semgrep_output_v1_t.location) =
  let file_buffer = Common.read_file loc.path in
  let file_lines = Str.split (Str.regexp "\n") file_buffer in
  let line = List.nth file_lines (loc.start.line - 1) in
  let prev_line =
    if loc.start.line > 1 then List.nth file_lines (loc.start.line - 2) else ""
  in
  prev_line ^ line

(* TODO: Move to Nosemgrep.ml to factorize code *)

(** Check if a match is ignored by a nosem comment *)
let nosem_ignored (loc : Semgrep_output_v1_t.location) rule_id =
  let line = get_match_lines loc in
  let matched_inline = SPcre.exec ~rex:Nosemgrep.nosem_inline_re line in
  let matched_prev = SPcre.exec ~rex:Nosemgrep.nosem_previous_line_re line in
  let match_ok m =
    match m with
    | Ok (Some substrings) -> Pcre.get_substring substrings 1 = rule_id
    | Ok None
    | Error _ ->
        false
  in
  let match_ok m =
    try match_ok m with
    | Not_found -> true
  in
  let matched_inline = match_ok matched_inline in
  let matched_prev = match_ok matched_prev in
  matched_inline || matched_prev

(** Replaces metavar placeholders in text with their value
    TODO: Factorize with Cli_json_output.interpolate_metavars()
*)
let interpolate_metavars (metavars : Semgrep_output_v1_t.metavars) text =
  Common2.fold
    (fun text ((l, v) : string * Semgrep_output_v1_t.metavar_value) ->
      let re = Str.regexp_string l in
      Str.global_replace re v.abstract_content text)
    text metavars

(* Get fix from rule, then make it make sense in context.
   TODO: Factorize with Cli_json_output.render_fix()
*)
let convert_fix (m : Semgrep_output_v1_t.core_match) (rule : Rule.t) =
  let rule_fix (r : Rule.t) =
    match r.fix with
    | Some fix -> Some (interpolate_metavars m.extra.metavars fix)
    (*TODO: regex autofix*)
    | None -> None
  in
  let fix =
    match m.extra.rendered_fix with
    | Some fix -> Some fix
    | None -> rule_fix rule
  in
  fix

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

let of_matches ?(only_git_dirty = true) matches (hrules : Rule.hrules) files =
  let matches, _ =
    Common.partition_either
      (JSON_report.match_to_match (Some Autofix.render_fix))
      matches
  in
  (* Match up the rules with the matches so we can get fixes, rule ids, messages *)
  let matches =
    Common.map
      (fun (m : Semgrep_output_v1_t.core_match) ->
        let rule = Hashtbl.find_opt hrules (Rule.ID.of_string m.rule_id) in
        let rule =
          match rule with
          | Some rule -> rule
          | None -> failwith ("Rule " ^ m.rule_id ^ " not found")
        in
        let message =
          Some (interpolate_metavars m.extra.metavars rule.Rule.message)
        in
        let rendered_fix = convert_fix m rule in
        let m = { m with extra = { m.extra with rendered_fix; message } } in
        (m, rule))
      matches
  in
  let git_repo = Git_wrapper.is_git_repo () in
  (* Filter dirty lines *)
  let matches =
    if only_git_dirty && git_repo then filter_dirty_lines files matches
    else matches
  in
  (* Filter misc severities *)
  let matches =
    Common2.filter
      (fun ((_, r) : t) ->
        r.severity <> Rule.Experiment && r.severity <> Rule.Inventory)
      matches
  in
  (* Filter nosem comments. We should do this conditionally at some point *)
  let matches =
    Common2.filter
      (fun ((m, _) : t) -> not (nosem_ignored m.location m.rule_id))
      matches
  in
  (matches, files)
