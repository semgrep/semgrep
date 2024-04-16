open Common
open Semgrep_output_v1_t

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

let handle_call : function_call -> (function_return, string) result = function
  | `CallApplyFixes { dryrun; edits } ->
      let modified_file_count, fixed_lines = handle_autofix dryrun edits in
      Ok (`RetApplyFixes { modified_file_count; fixed_lines })
  | `CallSarifFormat _ -> Error "TODO: CallSarifFormat"

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
let handle_single_request () =
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
    try handle_call call with
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

let main _caps =
  (* For now, just handle one request and then exit. *)
  handle_single_request ()
