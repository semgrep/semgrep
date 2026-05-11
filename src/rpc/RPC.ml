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
open Common
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* OCaml side of the Python -> OCaml RPC
 *
 * See RPC_return.ml for the code implementing the Python RPC calls.
 *)

let name_of_call (call : Out.function_call) : string =
  match call with
  | `CallApplyFixes _ -> "CallApplyFixes"
  | `CallSarifFormat _ -> "CallSarifFormat"
  | `CallContributions -> "CallContributions"
  | `CallFormatter _ -> "CallFormatter"
  | `CallValidate _ -> "CallValidate"
  | `CallResolveDependencies _ -> "CallResolveDependencies"
  | `CallUploadSymbolAnalysis _ -> "CallUploadSymbolAnalysis"
  | `CallDumpRulePartitions _ -> "CallDumpRulePartitions"
  | `CallTransitiveReachabilityFilter _ -> "CallTransitiveReachabilityFilter"
  | `CallGetTargets _ -> "CallGetTargets"
  | `CallMatchSubprojects _ -> "CallMatchSubprojects"
  | `CallRunSymbolAnalysis _ -> "CallRunSymbolAnalysis"
  | `CallUploadSubprojectSymbolAnalysis _ ->
      "CallUploadSubprojectSymbolAnalysis"
  | `CallShowSubprojects _ -> "CallShowSubprojects"

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type config = { par_conf : Parallelism_config.t; num_jobs : int }
[@@deriving show]
(*****************************************************************************)
(* Dispatcher *)
(*****************************************************************************)

let handle_call (conf : config) (call : Out.function_call) :
    (Out.function_return, string) result =
  Profiling.measure ("RPC " ^ name_of_call call) @@ fun () ->
  match call with
  | `CallApplyFixes { dryrun; edits } ->
      let modified_file_count, fixed_lines = RPC_return.autofix dryrun edits in
      Ok
        (`RetApplyFixes { modified_file_count; fixed_lines }
          : Out.function_return)
  | `CallSarifFormat ({ rules; is_pro; show_dataflow_traces }, ctx, cli_output)
    ->
      let output =
        RPC_return.sarif_format rules ctx ~is_pro ~show_dataflow_traces
          cli_output
      in
      Ok (`RetSarifFormat output)
  | `CallContributions ->
      let contribs = RPC_return.contributions () in
      Ok (`RetContributions contribs)
  | `CallFormatter (output_format, ctx, cli_output) ->
      let str = RPC_return.format output_format ctx cli_output in
      Ok (`RetFormatter str)
  | `CallValidate path ->
      let valid =
        RPC_return.validate ~par_conf:conf.par_conf ~num_jobs:conf.num_jobs path
      in
      Ok (`RetValidate valid)
  | `CallResolveDependencies params -> (
      match !RPC_return.hook_resolve_dependencies with
      | Some resolve_dependencies ->
          let package_manager_env =
            Option.value ~default:[] params.package_manager_env
          in
          let resolved =
            resolve_dependencies ~par_conf:conf.par_conf
              ~download_dependency_source_code:
                params.download_dependency_source_code
              ~allow_local_builds:params.allow_local_builds ~package_manager_env
              params.dependency_sources
          in
          Ok (`RetResolveDependencies resolved)
      | None ->
          Error
            "Dependency resolution is a proprietary feature, but semgrep-pro \
             has not been loaded")
  | `CallUploadSymbolAnalysis (token, scan_id, symbol_analysis) -> (
      (* Caps are kind of a crap shoot whyen working across programming language
         boundaries anyways.
      *)
      let token = Auth.unsafe_token_of_string token in
      match
        Semgrep_App.upload_symbol_analysis ~token ~scan_id symbol_analysis
      with
      | Error msg -> Error msg
      | Ok msg -> Ok (`RetUploadSymbolAnalysis msg))
  | `CallDumpRulePartitions params -> (
      match !RPC_return.hook_dump_rule_partitions with
      | Some dump_rule_partitions ->
          let ok = dump_rule_partitions params in
          Ok (`RetDumpRulePartitions ok)
      | None ->
          Error
            "Dump rule partitions is a proprietary feature, but semgreep-pro \
             has not been loaded")
  | `CallTransitiveReachabilityFilter params -> (
      match !RPC_return.hook_transitive_reachability_analyzer with
      | Some transitive_reachability_filter ->
          let xs = transitive_reachability_filter params in
          Ok (`RetTransitiveReachabilityFilter xs)
      | None ->
          Error
            "Transitive reachability is a proprietary feature, but semgrep-pro \
             has not been loaded")
  | `CallGetTargets scanning_roots ->
      Ok
        (`RetGetTargets
           (Core_scan.get_targets_for_pysemgrep ~par_conf:conf.par_conf
              ~num_jobs:conf.num_jobs scanning_roots))
  | `CallMatchSubprojects params -> (
      match !RPC_return.hook_match_subprojects with
      | Some match_subprojects ->
          let xs = match_subprojects params in
          Ok (`RetMatchSubprojects xs)
      | None ->
          Error
            "Subproject matching is a proprietary feature, but semgrep-pro has \
             not been loaded")
  | `CallRunSymbolAnalysis params -> (
      let msg =
        "Symbol analysis is a proprietary feature, but semgrep-pro has not \
         been loaded"
      in
      let/ run_symbol_analysis =
        Option.to_result ~none:msg !RPC_return.hook_run_symbol_analysis
      in
      match run_symbol_analysis params with
      | Ok analysis -> Ok (`RetRunSymbolAnalysis analysis)
      | Error msg -> Error msg)
  | `CallUploadSubprojectSymbolAnalysis
      { token; scan_id; manifest; lockfile; symbol_analysis } -> (
      let token = Auth.unsafe_token_of_string token in
      match
        Semgrep_App.upload_subproject_symbol_analysis ~token ~scan_id ~manifest
          ~lockfile symbol_analysis
      with
      | Error msg -> Error msg
      | Ok msg -> Ok (`RetUploadSubprojectSymbolAnalysis msg))
  | `CallShowSubprojects subprojects ->
      Ok
        (`RetShowSubprojects
           ((Plugin.get Show_subcommand.pro_sca_output_functions_plugin)
              .show_subprojects subprojects))

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type header = Stop | Size of int

(* Read the "header" of a message, which is the message's size in
   bytes followed by \n.

   This will return Stop if we encounter an EOF before \n, even if there
   were other characters written to the stream. This runs the risk of
   masking actual problems (ie a process that got interrupted in the
   middle of writing the header), but I couldn't figure out a reasonable
   way to handle this case using the input functions in Stdlib.*)
let read_header chan_in =
  let parse_header header =
    match int_of_string_opt header with
    | Some i -> Ok (Size i)
    | None ->
        let truncated = String_.safe_sub header 0 50 in
        Error
          (spf "Error decoding RPC request: expected integer, got '%s'"
             truncated)
  in
  try parse_header (input_line chan_in) with
  | End_of_file -> Ok Stop

let read_packet chan_in size =
  try Ok (really_input_string chan_in size) with
  | End_of_file -> Error "Reached EOF while reading RPC request"

let write_packet chan_out str =
  let size = String.length str in
  let size_str = string_of_int size in
  output_string chan_out size_str;
  output_char chan_out '\n';
  output_string chan_out str;
  flush chan_out

(* Blocks until a request comes in, then handles it and sends the result back *)
let handle_request conf chan_in chan_out size =
  let res =
    let/ call_str = read_packet chan_in size in
    let/ rpc_call =
      try Ok (Semgrep_output_v1_j.rpc_call_of_string call_str) with
      | e ->
          let e = Exception.catch e in
          Error (spf "Error parsing RPC request:\n%s" (Exception.to_string e))
    in
    Tracing.with_top_level_span ?parent_span_id:rpc_call.Out.parent_span_id
      ~__FILE__ ~__LINE__
      ("RPC " ^ name_of_call rpc_call.Out.call)
    @@ fun _scope ->
    try handle_call conf rpc_call.Out.call with
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
  let result : Out.function_result =
    {
      function_return = func_return;
      profiling_results = Core_json_output.export_simple_profiling_results ();
    }
  in
  let res_str = Semgrep_output_v1_j.string_of_function_result result in
  write_packet chan_out res_str

let rec handle_multiple_requests conf chan_in chan_out =
  match read_header chan_in with
  | Ok Stop -> ()
  | Ok (Size size) -> begin
      handle_request conf chan_in chan_out size;
      handle_multiple_requests conf chan_in chan_out
    end
  | Error str ->
      write_packet chan_out
        (Semgrep_output_v1_j.string_of_function_return (`RetError str))

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* For now, just handle one request and then exit. *)
let main conf = handle_multiple_requests conf stdin stdout [@@profiling]
