val range_of_cli_match : Semgrep_output_v1_t.cli_match -> Lsp.Types.Range.t
(** [range_of_cli_match cli_match] returns the lsp range of the given cli_match. *)

val convert_severity :
  Semgrep_output_v1_t.match_severity -> Lsp.Types.DiagnosticSeverity.t
(** [convert_severity s] returns the lsp severity corresponding to the semgrep severity [s]. *)

val workspace_folders_to_paths :
  Lsp.Types.WorkspaceFolder.t list -> Fpath.t list
(** [workspace_folders_to_paths folders] returns the list of paths of the given workspace folders. *)
