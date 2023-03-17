open Lsp
open Types

let range_of_location (loc : Semgrep_output_v1_t.location) =
  Range.create
    ~start:
      (Position.create ~line:(loc.start.line - 1) ~character:(loc.start.col - 1))
    ~end_:
      (Position.create ~line:(loc.end_.line - 1) ~character:(loc.end_.col - 1))

let code_action_of_match (m : Semgrep_output_v1_t.core_match) fix =
  let edit =
    WorkspaceEdit.create
      ~changes:
        [
          ( Uri.of_path m.location.path,
            [
              TextEdit.create ~range:(range_of_location m.location) ~newText:fix;
            ] );
        ]
      ()
  in
  CodeAction.create
    ~title:(Printf.sprintf "Apply fix suggested by Semgrep rule %s" m.rule_id)
    ~kind:CodeActionKind.QuickFix
    ~diagnostics:[]
    ~edit
