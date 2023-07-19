val code_actions_of_cli_matches :
  Semgrep_output_v1_t.cli_match list ->
  Fpath.t list ->
  [> `CodeAction of Lsp.Types.CodeAction.t ] list
(** [code_actions_of_cli_matches matches files] returns a list of code actions
    for the given matches. *)
