open Lsp
open Types
module OutJ = Semgrep_output_v1_t

let range_of_cli_match (m : OutJ.cli_match) =
  Range.create
    ~start:
      (Position.create ~line:(m.start.line - 1) ~character:(m.start.col - 1))
    ~end_:(Position.create ~line:(m.end_.line - 1) ~character:(m.end_.col - 1))

let range_of_toks ((l1 : Tok.location), (l2 : Tok.location)) =
  let line, col, _ = Tok.end_pos_of_loc l2 in
  Range.create
    ~start:(Position.create ~line:(l1.pos.line - 1) ~character:l1.pos.column)
    ~end_:(Position.create ~line:(line - 1) ~character:col)

let convert_severity (severity : OutJ.match_severity) : DiagnosticSeverity.t =
  match severity with
  (* no notion of Critical in LSP *)
  | `Error
  | `Critical
  | `High ->
      Error
  | `Warning
  | `Medium ->
      Warning
  | `Info
  | `Low
  | `Experiment
  | `Inventory ->
      Information

let workspace_folders_to_paths =
  List_.map (fun ({ uri; _ } : WorkspaceFolder.t) -> Uri.to_path uri |> Fpath.v)
