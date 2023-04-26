val diagnostics_of_results :
  Processed_run.t list -> string list -> Lsp.Server_notification.t list
(** Produces LSP diagnostics from the run results, for the specified files*)
