(* internal only *)
type conf = {
  merge_partial_results_dir : Fpath.t option;
  merge_partial_results_output : Fpath.t option;
  validate_partial_results_expected : Fpath.t option;
  validate_partial_results_actual : Fpath.t option;
}
[@@deriving show]

let hook_pro_read_and_merge_partial_scan_results :
    (input_dir:Fpath.t -> output_json:Fpath.t -> unit) option ref =
  ref None

let hook_pro_read_and_validate_partial_scan_results :
    (expected:Fpath.t -> actual:Fpath.t -> bool) option ref =
  ref None

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
      match !hook_pro_read_and_merge_partial_scan_results with
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
      match !hook_pro_read_and_validate_partial_scan_results with
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
