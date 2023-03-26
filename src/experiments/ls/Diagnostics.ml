open Lsp
open Types

(*let logger = Logging.get_logger [ __MODULE__ ]*)

let range_of_location (loc : Semgrep_output_v1_t.location) =
  Range.create
    ~start:
      (Position.create ~line:(loc.start.line - 1) ~character:(loc.start.col - 1))
    ~end_:
      (Position.create ~line:(loc.end_.line - 1) ~character:(loc.end_.col - 1))

let string_of_severity (severity : Rule.severity) =
  match severity with
  | Error -> DiagnosticSeverity.Error
  | Warning -> DiagnosticSeverity.Warning
  | Inventory
  | Experiment
  | Info -> DiagnosticSeverity.Information

let diagnostic_of location severity message =
  Diagnostic.create
    ~range:(range_of_location location)
    ~severity ~source:"Semgrep" ~message ()

let diagnostic_of_match (m, r : Semgrep_output_v1_t.core_match * Rule.rule) =
  let message = r.message in
  let severity = string_of_severity r.severity in
  diagnostic_of m.location severity message

let diagnostics_of_file (matches : (Semgrep_output_v1_t.core_match * Rule.rule) list) file =
  let matches =
    Common2.filter
      (fun (m,_ : Semgrep_output_v1_t.core_match* Rule.rule) -> m.location.path = file)
      matches
  in
  let diagnostics = Common.map diagnostic_of_match matches in
  let params =
    PublishDiagnosticsParams.create
      ~uri:
        (Uri.of_path file)
      ~diagnostics ()
  in
  Server_notification.PublishDiagnostics params

let diagnostics_of_results results files =
  Common.map (diagnostics_of_file results) files
