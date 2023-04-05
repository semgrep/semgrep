open Git_helper

type t = Semgrep_output_v1_t.core_match * Rule.rule

(* This is copied from osemgrep/Nosem.ml, when porting is done we should use
   stuff from there *)

let rule_id_re_str = {|(?:[:=][\s]?(?P<ids>([^,\s](?:[,\s]+)?)+))?|}
let nosem_inline_re_str = {| nosem(?:grep)?|} ^ rule_id_re_str
let _nosem_inline_re = SPcre.regexp nosem_inline_re_str ~flags:[ `CASELESS ]

let _nosem_previous_line_re =
  SPcre.regexp
    ({|^[^a-zA-Z0-9]* nosem(?:grep)?|} ^ rule_id_re_str)
    ~flags:[ `CASELESS ]

let filter_dirty_lines files matches =
  let dirty_files = Hashtbl.create 10 in
  let%lwt () =
    Lwt_list.iter_s
      (fun f ->
        let%lwt dirty_lines = dirty_lines_of_file f in
        Hashtbl.add dirty_files f dirty_lines;
        Lwt.return ())
      files
  in
  Lwt_list.filter_p
    (fun ((m, _) : t) ->
      let dirty_lines = Hashtbl.find_opt dirty_files m.location.path in
      let line = m.location.start.line in
      let res =
        match dirty_lines with
        | None -> false (* Untracked files *)
        | Some [||] -> true
        | Some dirty_lines ->
            Array.exists
              (fun (start, end_) -> start <= line && line <= end_)
              dirty_lines
      in
      Lwt.return res)
    matches

let get_match_lines (loc : Semgrep_output_v1_t.location) =
  let file_buffer = Common.read_file loc.path in
  let file_lines = Str.split (Str.regexp "\n") file_buffer in
  let line = List.nth file_lines (loc.start.line - 1) in
  let prev_line =
    if loc.start.line > 1 then List.nth file_lines (loc.start.line - 2) else ""
  in
  prev_line ^ line

let nosem_ignored (loc : Semgrep_output_v1_t.location) rule_id =
  let line = get_match_lines loc in
  let matched_inline = SPcre.exec ~rex:_nosem_inline_re line in
  let matched_prev = SPcre.exec ~rex:_nosem_previous_line_re line in
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

let interpolate_metavars (metavars : Semgrep_output_v1_t.metavars) text =
  Common2.fold
    (fun text ((l, v) : string * Semgrep_output_v1_t.metavar_value) ->
      let re = Str.regexp_string l in
      Str.global_replace re v.abstract_content text)
    text metavars

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

let postprocess_results results hrules files =
  let results =
    JSON_report.match_results_of_matches_and_errors (Some Autofix.render_fix)
      (List.length files) results
  in
  let%lwt matches =
    Lwt_list.map_p
      (fun (m : Semgrep_output_v1_t.core_match) ->
        let rule = Hashtbl.find_opt hrules m.rule_id in
        let rule =
          match rule with
          | Some rule -> rule
          | None -> failwith ("Rule " ^ m.rule_id ^ " not found")
        in
        let m =
          {
            m with
            extra =
              {
                m.extra with
                rendered_fix = convert_fix m rule;
                message =
                  Some (interpolate_metavars m.extra.metavars rule.Rule.message);
              };
          }
        in
        Lwt.return (m, rule))
      results.matches
  in
  let%lwt git_repo = is_git_repo () in
  let%lwt matches =
    if git_repo then filter_dirty_lines files matches else Lwt.return matches
  in
  let%lwt matches =
    Lwt_list.filter_p
      (fun ((_, r) : t) ->
        Lwt.return
          (r.severity <> Rule.Experiment && r.severity <> Rule.Inventory))
      matches
  in
  let%lwt matches =
    Lwt_list.filter_p
      (fun ((m, _) : t) ->
        Lwt.return (not (nosem_ignored m.location m.rule_id)))
      matches
  in
  Lwt.return (matches, files)
