open Lsp
open Types

let range_of_cli_match (m : Semgrep_output_v1_t.cli_match) =
  Range.create
    ~start:
      (Position.create ~line:(m.start.line - 1) ~character:(m.start.col - 1))
    ~end_:(Position.create ~line:(m.end_.line - 1) ~character:(m.end_.col - 1))

let severity_of_string (severity : string) =
  match severity with
  | "ERROR" -> DiagnosticSeverity.Error
  | "WARNING" -> DiagnosticSeverity.Warning
  | _ -> DiagnosticSeverity.Information

let workspace_folders_to_paths =
  Common.map (fun ({ uri; _ } : WorkspaceFolder.t) ->
      Uri.to_path uri |> Fpath.v)
