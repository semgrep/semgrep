val diagnostics_of_results :
  Semgrep_output_v1_t.cli_match list ->
  Fpath.t list ->
  Lsp.Server_notification.t list
(** [diagnostics_of_results results files] returns a list of LSP diagnostics
    for the given matches. *)
