val range_of_location : Semgrep_output_v1_t.location -> Lsp.Types.Range.t
val string_of_severity : Rule.severity -> Lsp.Types.DiagnosticSeverity.t

val workspace_folders_to_paths :
  Lsp.Types.WorkspaceFolder.t list -> Fpath.t list
