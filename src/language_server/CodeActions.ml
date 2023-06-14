open Lsp
open Types
module Conv = Convert_utils

let code_action_of_match (m : Semgrep_output_v1_t.core_match) =
  let fix =
    match m.extra.rendered_fix with
    | Some fix -> fix
    | None -> failwith "no rendered fix"
  in

  let edit =
    WorkspaceEdit.create
      ~changes:
        [
          ( Uri.of_path m.location.path,
            [
              TextEdit.create
                ~range:(Conv.range_of_location m.location)
                ~newText:fix;
            ] );
        ]
      ()
  in
  let action =
    CodeAction.create
      ~title:(Printf.sprintf "Apply fix suggested by Semgrep rule %s" m.rule_id)
      ~kind:CodeActionKind.QuickFix ~edit ()
  in
  `CodeAction action

let code_actions_of_file (matches : Semgrep_output_v1_t.core_match list) file =
  let matches =
    List.filter
      (fun (m : Semgrep_output_v1_t.core_match) ->
        m.location.path = file && m.extra.rendered_fix <> None)
      matches
  in
  Common.map code_action_of_match matches

let code_actions_of_core_matches matches files =
  List.concat_map (code_actions_of_file matches) files
