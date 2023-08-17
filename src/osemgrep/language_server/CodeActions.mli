val code_actions_of_cli_matches :
  Semgrep_output_v1_t.cli_match list ->
  Fpath.t list ->
  [> `CodeAction of Lsp.Types.CodeAction.t ] list
(** [code_actions_of_cli_matches matches files] returns a list of code actions
    for the given matches. A code action gives the user the option to execute
    a command when hovering over a finding. We use this to provide autofix
    options to the user.
    See:
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_codeAction
*)
(* Example *)
(* A match that has an autofix will produce a diagnostic like this:
   {
     "edit": {
         "changes": {
             "file.py": [
                 {
                     "newText": "SHA256",
                     "range": {
                         "end": {
                             "character": 11,
                             "line": 9
                         },
                         "start": {
                             "character": 7,
                             "line": 9
                         }
                     }
                 }
             ]
         }
     },
     "kind": "quickfix",
     "title": "Apply fix suggested by Semgrep rule python.cryptography.security.insecure-hash-algorithms.insecure-hash-algorithm-sha1"
   }
*)
