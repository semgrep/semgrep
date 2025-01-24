(* Austin Theriault
 *
 * Copyright (C) 2019-2023 Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
(* Commentary *)
(* Code actions are what's presented to a user when they hover over a *)
(* (highlighted) piece of code. *)
(* coupling: if adding a new command, add it to Execute_command *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
open Fpath_.Operators
open Lsp
open Types
open RPC_server
module CN = Client_notification
module CR = Client_request
module Conv = Convert_utils
module OutJ = Semgrep_output_v1_t

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fix_code_action_of_match (m : OutJ.cli_match) =
  let fix =
    match m.extra.fix with
    | Some fix -> fix
    | None -> failwith "no rendered fix"
  in

  let edit =
    WorkspaceEdit.create
      ~changes:
        [
          ( Uri.of_path !!(m.path),
            [ TextEdit.create ~range:(Conv.range_of_cli_match m) ~newText:fix ]
          );
        ]
      ()
  in
  let command = Autofix_cmd.create () in
  let action =
    CodeAction.create ~command
      ~title:
        (Printf.sprintf "Semgrep(%s): Apply autofix"
           (Rule_ID.to_string m.check_id))
      ~isPreferred:true ~kind:CodeActionKind.QuickFix ~edit ()
  in
  `CodeAction action

let ignore_code_action_of_match (m : OutJ.cli_match) =
  let command =
    Ignore_cmd.create ~path:(Fpath.to_string m.path)
      ~fingerprint:m.extra.fingerprint
  in
  let action =
    CodeAction.create ~command
      ~title:
        Printf.(
          sprintf "Semgrep(%s): Ignore finding" (Rule_ID.to_string m.check_id))
      ~isPreferred:true ~kind:CodeActionKind.QuickFix ()
  in
  `CodeAction action

let code_actions_of_match (m : OutJ.cli_match) =
  let ignore_action = ignore_code_action_of_match m in
  match m.extra.fix with
  | Some _ -> [ fix_code_action_of_match m; ignore_action ]
  | None -> [ ignore_action ]

let code_actions_of_file (matches : OutJ.cli_match list) file =
  let matches =
    List.filter (fun (m : OutJ.cli_match) -> m.path = file) matches
  in
  List.concat_map code_actions_of_match matches

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

(** [code_actions_of_cli_matches matches files] returns a list of code actions
    for the given matches. A code action gives the user the option to execute
    a command when hovering over a finding. We use this to provide autofix
    options to the user.
    See:
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_codeAction
*)
let code_actions_of_cli_matches (matches : OutJ.cli_match list)
    (files : Fpath.t list) : [> `CodeAction of Lsp.Types.CodeAction.t ] list =
  List.concat_map (code_actions_of_file matches) files

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let on_request server
    ({ textDocument = { uri }; context; _ } : CodeActionParams.t) =
  let file = uri |> Uri.to_path |> Fpath.v in
  let matches =
    let ranges =
      List_.map (fun (d : Diagnostic.t) -> d.range) context.diagnostics
    in
    Session.previous_scan_of_file server.session file
    |> Option.value ~default:[]
    |> Common2.filter (fun (m : Semgrep_output_v1_t.cli_match) ->
           List.mem (Conv.range_of_cli_match m) ranges)
  in
  let actions = code_actions_of_cli_matches matches [ file ] in
  (server, Some actions)
