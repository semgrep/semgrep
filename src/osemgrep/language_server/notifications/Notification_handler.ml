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
(*  *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
open Lsp
open Types
open Jsonrpc
open Yojson.Safe.Util
module CN = Client_notification
module Conv = Convert_utils

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

(* Dispatch to the various custom request handlers. *)
let handle_custom_notification server (meth : string)
    (params : Jsonrpc.Structured.t option) : RPC_server.t =
  match
    [ (LoginFinish.meth, LoginFinish.on_notification) ] |> List.assoc_opt meth
  with
  | None ->
      Logs.warn (fun m -> m "Unhandled custom notification %s" meth);
      server
  | Some handler ->
      handler server params;
      server

let on_notification notification (server : RPC_server.t) =
  let server =
    match notification with
    | _ when server.state = RPC_server.State.Uninitialized -> server
    | CN.Initialized ->
        Logs.debug (fun m -> m "Server initialized");
        Scan_helpers.refresh_rules server;
        server
    | CN.TextDocumentDidChange
        { textDocument = { uri; _ }; contentChanges = first :: _ } ->
        Logs.debug (fun m ->
            m "Scanning file %s on change " (Uri.to_string uri));
        let content = Some first.text in
        Scan_helpers.scan_file ~content server uri;
        server
    | CN.DidSaveTextDocument { textDocument = { uri }; _ } ->
        Logs.debug (fun m -> m "Scanning file %s on save" (Uri.to_string uri));
        Scan_helpers.scan_file server uri;
        server
    | CN.TextDocumentDidOpen { textDocument = { uri; _ } } ->
        let path = uri |> Uri.to_path |> Fpath.v in
        let prev_scan = Session.previous_scan_of_file server.session path in
        (* We usually scan every file on startup, so let's only rescan an opened
            file if there weren't previous results *)
        if Option.is_some prev_scan then
          Logs.debug (fun m ->
              m "File %s already scanned, not rescanning" (Uri.to_string uri))
        else (
          Logs.debug (fun m -> m "Scanning file %s on open" (Uri.to_string uri));
          Scan_helpers.scan_file server uri);
        server
    | CN.ChangeWorkspaceFolders { event = { added; removed }; _ } ->
        let session =
          let added = Conv.workspace_folders_to_paths added in
          let removed = Conv.workspace_folders_to_paths removed in
          Session.update_workspace_folders server.session ~added ~removed
        in
        let server = { server with session } in
        Scan_helpers.scan_workspace server;
        server
    | CN.DidDeleteFiles { files; _ } ->
        (* This is lame, for whatever reason they chose to type uri as string here, not Uri.t *)
        let diagnostics =
          let files =
            Common.map
              (fun { FileDelete.uri } ->
                Str.string_after uri (String.length "file://") |> Fpath.v)
              files
          in
          Diagnostics.diagnostics_of_results
            ~is_intellij:server.session.is_intellij [] files
        in
        RPC_server.batch_notify diagnostics;
        server
    | CN.Exit ->
        Logs.debug (fun m -> m "Server exiting");
        { server with state = RPC_server.State.Stopped }
    | CN.UnknownNotification { method_ = "semgrep/refreshRules"; _ } ->
        Scan_helpers.refresh_rules server;
        server
    | CN.UnknownNotification { method_ = "semgrep/logout"; _ } ->
        if
          Semgrep_settings.save
            { (Semgrep_settings.load ()) with api_token = None }
        then (
          RPC_server.notify_show_message ~kind:MessageType.Info
            "Logged out of Semgrep Code";
          server)
        else (
          RPC_server.notify_show_message ~kind:MessageType.Error
            "Failed to log out";
          server)
    | CN.UnknownNotification
        { method_ = "semgrep/scanWorkspace"; params = Some json } ->
        let full =
          Structured.yojson_of_t json
          |> member "full" |> to_bool_option
          |> Option.value ~default:false
        in
        if server.session.metrics.isNewAppInstall && full then
          RPC_server.notify_show_message ~kind:MessageType.Info
            "Scanning all files regardless of git status. These diagnostics \
             will persist until a file is edited. To default to always \
             scanning regardless of git status, please disable 'Only Git \
             Dirty' in settings";
        Logs.debug (fun m -> m "Scanning workspace, full: %b" full);
        let scan_server =
          let session =
            {
              server.session with
              user_settings =
                { server.session.user_settings with only_git_dirty = not full };
            }
          in
          { server with session }
        in
        Scan_helpers.scan_workspace scan_server;
        Logs.debug (fun m -> m "Scanning workspace complete");
        server
    | CN.UnknownNotification { method_; params } ->
        handle_custom_notification server method_ params
    | _ ->
        Logs.warn (fun m ->
            m "Unhandled notification %s"
              (CN.to_jsonrpc notification |> Notification.yojson_of_t
             |> Yojson.Safe.pretty_to_string));
        server
  in
  server
