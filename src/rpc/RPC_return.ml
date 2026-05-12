(*
   Copyright (c) 2024-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* The answer to the call from Python *)
(*****************************************************************************)

let autofix (dryrun : bool) (edits : Out.edit list) :
    int * (int * string list) list =
  let edits =
    edits
    |> List.map (fun Out.{ path; start_offset; end_offset; replacement_text } ->
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
      List.mapi (fun i edit -> (i, Fixed_lines.make_fixed_lines env edit)) edits
    in
    let fixed_lines =
      List.filter_map
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

let format (kind : Out.output_format) (ctx : Out.format_context)
    (cli_output : Out.cli_output) : string =
  let xs = Output.format kind ctx cli_output in
  String.concat "\n" xs

let sarif_format (rules : Out.fpath) (ctx : Out.format_context) ~is_pro
    ~show_dataflow_traces (cli_output : Out.cli_output) : string =
  let fake_config =
    {
      Core_scan_config.default with
      rule_source = Core_scan_config.Rule_files [ rules ];
    }
  in
  let rules, invalid_rules = Core_scan.rules_of_config fake_config in
  (* we already use Log.warn in Parse_rule.ml but worth repeating with Logs
   * TODO? where do the RPC logs go? using --debug does not show RPCs
   * logs; only failures are visible.
   *)
  if not (List_.null invalid_rules) then
    (* nosemgrep: no-logs-in-library *)
    Logs.warn (fun m ->
        m "skipping %d invalid rules in SARIF RPC" (List.length invalid_rules));
  let hrules = Rule.hrules_of_rules rules in
  let sarif_json =
    Sarif_output.sarif_output hrules ctx cli_output ~is_pro
      ~show_dataflow_traces
  in
  Sarif.Sarif_v_2_1_0_j.string_of_sarif_json_schema sarif_json

let contributions () : Out.contributions =
  Parse_contribution.get_contributions ()

let validate ~par_conf ~num_jobs (path : Out.fpath) : Out.core_error option =
  try
    let res = Parse_rule.parse ~par_conf ~num_jobs path in
    let valid =
      match res with
      | Ok _ -> None
      | Error e ->
          let error =
            e |> Core_error.error_of_rule_error
            |> Core_json_output.error_to_error
          in
          Some error
    in
    valid
  with
  | e ->
      let msg =
        Printf.sprintf "Error parsing rule file %s: %s" (Fpath.to_string path)
          (Printexc.to_string e)
      in
      let error =
        Core_error.mk_error ~msg
          ~loc:(Loc.first_loc_of_file path)
          Out.OtherParseError
        |> Core_json_output.error_to_error
      in
      Some error

(*****************************************************************************)
(* Hooks for handlers defined in Pro_RPC_return.ml *)
(*****************************************************************************)
(* nosemgrep: no-ref-declarations-at-top-scope *)
let hook_resolve_dependencies = ref None

(* nosemgrep: no-ref-declarations-at-top-scope *)
let hook_dump_rule_partitions = ref None

(* nosemgrep: no-ref-declarations-at-top-scope *)
let hook_transitive_reachability_analyzer = ref None

(* nosemgrep: no-ref-declarations-at-top-scope *)
let hook_match_subprojects = ref None

(* nosemgrep: no-ref-declarations-at-top-scope *)
let hook_run_symbol_analysis = ref None
