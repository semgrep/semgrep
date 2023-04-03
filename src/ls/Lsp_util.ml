open Lsp
open Types

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
  | Info ->
      DiagnosticSeverity.Information
