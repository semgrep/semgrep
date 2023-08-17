open Lsp
open Types
module Conv = Convert_utils
module Out = Semgrep_output_v1_t

let code_action_of_match (m : Out.cli_match) =
  let fix =
    match m.extra.fix with
    | Some fix -> fix
    | None -> failwith "no rendered fix"
  in

  let edit =
    WorkspaceEdit.create
      ~changes:
        [
          ( Uri.of_path m.path,
            [ TextEdit.create ~range:(Conv.range_of_cli_match m) ~newText:fix ]
          );
        ]
      ()
  in
  let action =
    CodeAction.create
      ~title:
        (Printf.sprintf "Apply fix suggested by Semgrep rule %s" m.check_id)
      ~kind:CodeActionKind.QuickFix ~edit ()
  in
  `CodeAction action

let code_actions_of_file (matches : Out.cli_match list) file =
  let matches =
    List.filter
      (fun (m : Out.cli_match) -> Fpath.v m.path = file && m.extra.fix <> None)
      matches
  in
  Common.map code_action_of_match matches

let code_actions_of_cli_matches matches files =
  List.concat_map (code_actions_of_file matches) files
