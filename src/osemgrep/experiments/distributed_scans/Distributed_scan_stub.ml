(* internal only *)
type conf = {
  merge_partial_results_dir : Fpath.t option;
  merge_partial_results_output : Fpath.t option;
  validate_partial_results_expected : Fpath.t option;
  validate_partial_results_actual : Fpath.t option;
  upload_partial_results : Fpath.t option;
  upload_partial_results_scan_id : int option;
}
[@@deriving show]

let hook_pro_read_and_merge_partial_scan_results :
    (input_dir:Fpath.t -> output_json:Fpath.t -> unit) option Hook.t =
  Hook.create None

let hook_pro_read_and_validate_partial_scan_results :
    (expected:Fpath.t -> actual:Fpath.t -> bool) option Hook.t =
  Hook.create None

let hook_pro_read_and_upload_partial_scan_results :
    (< Cap.network ; Auth.cap_token > ->
    scan_id:int ->
    partial_results:Fpath.t ->
    bool)
    option
    Hook.t =
  Hook.create None

let maybe_merge_partial_scan_results_then_exit (conf : conf) =
  match (conf.merge_partial_results_dir, conf.merge_partial_results_output) with
  | Some _, None
  | None, Some _ ->
      Logs.err (fun m ->
          m
            "Both or none of --x-merge-partial-results-dir and \
             --x-merge-partial-results-output must be present.");
      Error.exit_code_exn (Exit_code.fatal ~__LOC__)
  | None, None -> ()
  | Some input_dir, Some output_file -> (
      match Hook.get hook_pro_read_and_merge_partial_scan_results with
      | None ->
          Logs.err (fun m ->
              m
                "You have requested a setting that requires the pro engine, \
                 but do not have the pro engine installed.");
          Error.exit_code_exn (Exit_code.fatal ~__LOC__)
      | Some read_and_merge_partial_scan_results ->
          read_and_merge_partial_scan_results input_dir output_file;
          (* Not really an error, but abusing exit_code_exn for short circuiting *)
          Error.exit_code_exn (Exit_code.ok ~__LOC__))

let maybe_validate_partial_scan_results_then_exit (conf : conf) =
  match
    ( conf.validate_partial_results_expected,
      conf.validate_partial_results_actual )
  with
  | Some _, None
  | None, Some _ ->
      Logs.err (fun m ->
          m
            "Both or none of --x-validate-partial-results-actual and \
             --x-validate-partial-results-expected must be present.");
      Error.exit_code_exn (Exit_code.fatal ~__LOC__)
  | None, None -> ()
  | Some expected, Some actual -> (
      match Hook.get hook_pro_read_and_validate_partial_scan_results with
      | None ->
          Logs.err (fun m ->
              m
                "You have requested a setting that requires the pro engine, \
                 but do not have the pro engine installed.");
          Error.exit_code_exn (Exit_code.fatal ~__LOC__)
      | Some read_and_validate_partial_scan_results ->
          (* Abusing exit_code_exn for short circuiting, even for the non-error case. *)
          if read_and_validate_partial_scan_results expected actual then
            Error.exit_code_exn (Exit_code.ok ~__LOC__)
          else Error.exit_code_exn (Exit_code.fatal ~__LOC__))

let maybe_upload_partial_scan_results_then_exit
    (caps : < Cap.network ; Auth.cap_token >) (conf : conf) =
  match (conf.upload_partial_results, conf.upload_partial_results_scan_id) with
  | Some _, None
  | None, Some _ ->
      Logs.err (fun m ->
          m
            "Both or none of --x-upload_partial_scan_results and \
             --x-upload_partial_scan_results_scan_id must be present.");
      Error.exit_code_exn (Exit_code.fatal ~__LOC__)
  | None, None -> ()
  | Some partial_results, Some scan_id -> (
      match Hook.get hook_pro_read_and_upload_partial_scan_results with
      | None ->
          Logs.err (fun m ->
              m
                "You have requested a setting that requires the pro engine, \
                 but do not have the pro engine installed.");
          Error.exit_code_exn (Exit_code.fatal ~__LOC__)
      | Some read_and_upload_partial_scan_results ->
          (* Abusing exit_code_exn for short circuiting, even for the non-error case. *)
          if read_and_upload_partial_scan_results caps ~scan_id ~partial_results
          then Error.exit_code_exn (Exit_code.ok ~__LOC__)
          else Error.exit_code_exn (Exit_code.fatal ~__LOC__))
