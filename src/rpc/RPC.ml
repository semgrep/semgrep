open Common
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* OCaml side of the Python -> OCaml RPC
 *
 * See RPC_return.ml for the code implementing the Python RPC calls.
 *)

(*****************************************************************************)
(* Dispatcher *)
(*****************************************************************************)

let handle_call (caps : < Cap.exec ; Cap.tmp >) :
    Out.function_call -> (Out.function_return, string) result = function
  | `CallApplyFixes { dryrun; edits } ->
      let modified_file_count, fixed_lines = RPC_return.autofix dryrun edits in
      Ok (`RetApplyFixes { modified_file_count; fixed_lines })
  | `CallSarifFormat ({ rules; is_pro; show_dataflow_traces }, ctx, cli_output)
    ->
      let output =
        RPC_return.sarif_format
          (caps :> < Cap.tmp >)
          rules ctx ~is_pro ~show_dataflow_traces cli_output
      in
      Ok (`RetSarifFormat output)
  | `CallContributions ->
      let contribs = RPC_return.contributions (caps :> < Cap.exec >) in
      Ok (`RetContributions contribs)
  | `CallFormatter (output_format, ctx, cli_output) ->
      let str = RPC_return.format output_format ctx cli_output in
      Ok (`RetFormatter str)
  | `CallValidate path ->
      let valid = RPC_return.validate path in
      Ok (`RetValidate valid)
  | `CallResolveDependencies dependency_sources -> (
      match !RPC_return.hook_resolve_dependencies with
      | Some resolve_dependencies ->
          let resolved =
            resolve_dependencies
              (caps :> < Cap.exec ; Cap.tmp >)
              dependency_sources
          in
          Ok (`RetResolveDependencies resolved)
      | None ->
          Error
            "Dependency resolution is a proprietary feature, but semgrep-pro \
             has not been loaded")
  | `CallDumpRulePartitions params -> (
      match !RPC_return.hook_dump_rule_partitions with
      | Some dump_rule_partitions ->
          let Out.{ rules; n_partitions; output_dir } = params in
          let ok = dump_rule_partitions rules n_partitions output_dir in
          Ok (`RetDumpRulePartitions ok)
      | None ->
          Error
            "Dump rule partitions is a proprietary feature, but semgreep-pro \
             has not been loaded")

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let read_packet chan =
  let/ size_str =
    try Ok (input_line chan) with
    | End_of_file -> Error "Reached EOF while reading RPC request header"
  in
  let/ size =
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

(* Blocks until a request comes in, then handles it and sends the result back *)
let handle_single_request (caps : < Cap.exec ; Cap.tmp >) =
  let res =
    let/ call_str = read_packet stdin in
    let/ call =
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

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : < Cap.exec ; Cap.tmp >) =
  (* For some requests, such as SARIF formatting, we need to parse rules
   * so we need to init the parsers as well. *)
  Parsing_init.init ();

  (* For now, just handle one request and then exit. *)
  handle_single_request caps
