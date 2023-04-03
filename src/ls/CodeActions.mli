val code_actions_of_results :
  Semgrep_output_v1_t.core_match list ->
  string list ->
  [> `CodeAction of Lsp.Types.CodeAction.t ] list
