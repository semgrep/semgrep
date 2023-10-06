open Lsp
open Types
module Out = Semgrep_output_v1_t

let range_of_cli_match (m : Out.cli_match) =
  Range.create
    ~start:
      (Position.create ~line:(m.start.line - 1) ~character:(m.start.col - 1))
    ~end_:(Position.create ~line:(m.end_.line - 1) ~character:(m.end_.col - 1))

let convert_severity (severity : Out.rule_severity) : DiagnosticSeverity.t =
  match severity with
  | `Error -> Error
  | `Warning -> Warning
  | `Experiment
  | `Info
  | `Inventory ->
      Information

let workspace_folders_to_paths =
  Common.map (fun ({ uri; _ } : WorkspaceFolder.t) ->
      Uri.to_path uri |> Fpath.v)
