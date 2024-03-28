open Common
open Semgrep_output_v1_j

let ( let* ) = Result.bind

(* Once we get some more RPC calls we should probably move this to a different
 * file. But it seems fine for now. *)
let handle_autofix dryrun edits =
  let edits =
    edits
    |> List_.map (fun { path; start_offset; end_offset; replacement_text } ->
           Textedit.
             { path; start = start_offset; end_ = end_offset; replacement_text })
  in
  (* For a dry run, all we do is construct the fixed lines for each edit. This
   * makes it into the final JSON output. Otherwise, we write the edits to disk
   * and report the number of files that we modified. *)
  if dryrun then
    let fixed_lines_env = Autofix.make_fixed_lines_env () in
    (* We need to include the index of each edit along with its fixed_lines so
     * that the Python code can mutate the right match. *)
    let fixed_lines =
      List_.mapi
        (fun i edit -> (i, Autofix.make_fixed_lines fixed_lines_env edit))
        edits
    in
    let fixed_lines =
      List_.map_filter
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

let handle_sarif_format caps hide_nudge engine_label (rules : fpath)
    (cli_matches : cli_match list) (cli_errors : cli_error list) =
  let core_scan_conf =
    {
      Core_scan_config.default with
      rule_source = Some (Core_scan_config.Rule_file rules);
    }
  in
  let rules, _invalid_rules =
    Core_scan.rules_from_rule_source caps core_scan_conf
  in
  let hrules = Rule.hrules_of_rules rules in
  let cli_output =
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
          Sarif_output.sarif_output hide_nudge engine_label hrules cli_output
        in
        Sarif.Sarif_v_2_1_0_j.string_of_sarif_json_schema sarif_json)
  in
  (output, format_time_seconds)

let handle_call caps : function_call -> (function_return, string) result =
  function
  | `CallApplyFixes { dryrun; edits } ->
      let modified_file_count, fixed_lines = handle_autofix dryrun edits in
      Ok (`RetApplyFixes { modified_file_count; fixed_lines })
  | `CallSarifFormat
      { hide_nudge; engine_label; rules; cli_matches; cli_errors } ->
      let output, format_time_seconds =
        handle_sarif_format
          (caps :> < Cap.tmp >)
          hide_nudge engine_label rules cli_matches cli_errors
      in
      Ok (`RetSarifFormat { output; format_time_seconds })

let read_packet chan =
  let* size_str =
    try Ok (input_line chan) with
    | End_of_file -> Error "Reached EOF while reading RPC request header"
  in
  let* size =
    match int_of_string_opt size_str with
    | Some i -> Ok i
    | None ->
        let truncated = String_.safe_sub size_str 0 50 in
        Error
          (spf "Error decoding RPC request: expected integer, got '%s'"
             truncated)
  in
  try Ok (really_input_string chan size) with
  | End_of_file -> Error "Reached EOF while reading RPC request"

let write_packet chan str =
  let size = String.length str in
  let size_str = string_of_int size in
  output_string chan size_str;
  output_char chan '\n';
  output_string chan str;
  flush chan

(* Blocks until a request comes in, then handles it and sends the result back.
 * *)
let handle_single_request caps () =
  let res =
    let* call_str = read_packet stdin in
    let* call =
      try Ok (Semgrep_output_v1_j.function_call_of_string call_str) with
      (* It's not immediately clear what exceptions `function_call_of_string`
       * could raise on bad input. So let's be cautious and just handle
       * everything. *)
      | e ->
          let e = Exception.catch e in
          Error (spf "Error parsing RPC request:\n%s" (Exception.to_string e))
    in
    try handle_call caps call with
    (* Catch-all here. No matter what happens while handling this request, we
     * need to send a response back. *)
    | e ->
        let e = Exception.catch e in
        Error (spf "Error handling RPC request:\n%s" (Exception.to_string e))
  in
  let func_return =
    match res with
    | Ok func_return -> func_return
    | Error str -> `RetError str
  in
  let res_str = Semgrep_output_v1_j.string_of_function_return func_return in
  write_packet stdout res_str

let main caps =
  (* For now, just handle one request and then exit. *)
  handle_single_request caps ()
