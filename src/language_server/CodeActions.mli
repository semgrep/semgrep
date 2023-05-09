val code_actions_of_core_matches :
  Semgrep_output_v1_t.core_match list ->
  string list ->
  [> `CodeAction of Lsp.Types.CodeAction.t ] list
(** Takes a list of core matches, and what files to produce code actions for *)
