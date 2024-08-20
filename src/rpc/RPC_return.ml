module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* The answer to the call from Python *)
(*****************************************************************************)

let autofix (dryrun : bool) (edits : Out.edit list) :
    int * (int * string list) list =
  let edits =
    edits
    |> List_.map
         (fun Out.{ path; start_offset; end_offset; replacement_text } ->
           Textedit.
             { path; start = start_offset; end_ = end_offset; replacement_text })
  in
  (* For a dry run, all we do is construct the fixed lines for each edit. This
   * makes it into the final JSON output. Otherwise, we write the edits to disk
   * and report the number of files that we modified. *)
  if dryrun then
    let env = Fixed_lines.mk_env () in
    (* We need to include the index of each edit along with its fixed_lines so
     * that the Python code can mutate the right match. *)
    let fixed_lines =
      List_.mapi
        (fun i edit -> (i, Fixed_lines.make_fixed_lines env edit))
        edits
    in
    let fixed_lines =
      List_.filter_map
        (function
          | i, Some x -> Some (i, x)
          | _, None -> None)
        fixed_lines
    in
    (0, fixed_lines)
  else
    let modified_files, _failed_edit =
      Textedit.apply_edits ~dryrun:false edits
    in
    (List.length modified_files, [])

let format (kind : Out.output_format) (cli_output : Out.cli_output) : string =
  let kind' : Output_format.t =
    match kind with
    | `Vim -> Output_format.Vim
    | `Emacs -> Output_format.Emacs
  in
  let xs = Output.format kind' cli_output in
  String.concat "\n" xs

let sarif_format _caps hide_nudge engine_label show_dataflow_traces
    (rules : Out.fpath) (cli_matches : Out.cli_match list)
    (cli_errors : Out.cli_error list) =
  let rules, _invalid_rules =
    Core_scan.rules_from_rule_source (Core_scan_config.Rule_file rules)
  in
  let hrules = Rule.hrules_of_rules rules in
  let cli_output : Out.cli_output =
    {
      results = cli_matches;
      errors = cli_errors;
      (* The only fields that matter for sarif are cli_output.results and cli_output.errors,
       * so the rest of the fields are just populated with the minimal amount of info
       *)
      version = None;
      paths = { scanned = []; skipped = None };
      time = None;
      explanations = None;
      rules_by_engine = None;
      engine_requested = None;
      interfile_languages_used = None;
      skipped_rules = [];
    }
  in
  let output, format_time_seconds =
    Common.with_time (fun () ->
        let sarif_json =
          Sarif_output.sarif_output hide_nudge engine_label show_dataflow_traces
            hrules cli_output
        in
        Sarif.Sarif_v_2_1_0_j.string_of_sarif_json_schema sarif_json)
  in
  (output, format_time_seconds)

let contributions (caps : < Cap.exec >) : Out.contributions =
  Parse_contribution.get_contributions caps
