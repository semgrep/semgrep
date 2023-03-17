open Lsp
open Types

let lsp_severity_of_core_severity (severity : Semgrep_output_v1_t.core_severity)
    =
  match severity with
  | Semgrep_output_v1_t.Warning -> DiagnosticSeverity.Warning
  | Semgrep_output_v1_t.Error -> DiagnosticSeverity.Error

let range_of_location (loc : Semgrep_output_v1_t.location) =
  Range.create
    ~start:
      (Position.create ~line:(loc.start.line -1) ~character:(loc.start.col - 1))
    ~end_:
      (Position.create ~line:(loc.end_.line -1) ~character:(loc.end_.col - 1))

let diagnostic_of_match (m : Semgrep_output_v1_t.core_match) =
  let message =
    match m.extra.message with
    | Some s -> s
    | None -> "Match"
  in
  ignore message;
  Diagnostic.create
    ~range:(range_of_location m.location)
    ~severity:DiagnosticSeverity.Information
    ~source:"Semgrep"
    ~message:"TEST" ()

let diagnostic_of_error (e : Semgrep_output_v1_t.core_error) =
  let rule_id =
    match e.rule_id with
    | Some s -> s
    | None -> "Semgrep rule"
  in
  ignore rule_id;
  Diagnostic.create
    ~range:(range_of_location e.location)
    ~severity:(lsp_severity_of_core_severity e.severity)
    ~source:"Semgrep"
    ~message:"TEST" ()

let diagnostics_of_file (matches:Semgrep_output_v1_t.core_match list) (errors: Semgrep_output_v1_t.core_error list) file =
  let matches = Common2.filter (fun (m:Semgrep_output_v1_t.core_match) -> m.location.path = file) matches in
  let errors = Common2.filter (fun (e:Semgrep_output_v1_t.core_error) -> e.location.path = file) errors in
  let match_diagnostics = Common.map diagnostic_of_match matches in
  let error_diagnostics = Common.map diagnostic_of_error errors in
  let diagnostics = match_diagnostics @ error_diagnostics in
  let params = PublishDiagnosticsParams.create ~uri:(Uri.of_path (Printf.sprintf "/Users/r2cuser/r2c/tests/5007/%s" file)) ~diagnostics () in
  Server_notification.PublishDiagnostics params
  


let diagnostics_of_results matches files =
  let result =
    JSON_report.match_results_of_matches_and_errors (Some Autofix.render_fix)
      (List.length files) matches
  in
  Common.map (diagnostics_of_file result.matches result.errors) files
