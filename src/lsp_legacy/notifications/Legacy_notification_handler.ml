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
(* Handles all notifications from the client. There are none that are handled *)
(* when the server is uninitialized. Custom notifications should be setup here. *)
(* Notifications never return a response, but may trigger server to client *)
(* notifications or requests *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
open Lsp
open Legacy_lsp_
open Types
open Jsonrpc
open Yojson.Safe.Util
module CN = Client_notification
module Conv = Legacy_convert_utils

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

(* Dispatch to the various custom request handlers. *)
let handle_custom_notification session (meth : string)
    (params : Jsonrpc.Structured.t option) : Reply.t option =
  match [ (* Currently no custom notifications *) ] |> List.assoc_opt meth with
  | None ->
      Logs.warn (fun m -> m "Unhandled custom notification %s" meth);
      None
  | Some
      (handler :
        Legacy_session.t -> Jsonrpc.Structured.t option -> Legacy_lsp_.Reply.t)
    ->
      Some (handler session params)

let on_notification (server : Legacy_rpc_server.t) notification =
  Logs.debug (fun m ->
      m "Handling notification %s"
        (CN.to_jsonrpc notification |> Notification.yojson_of_t
       |> Yojson.Safe.pretty_to_string));
  let session = server.session in
  let server, reply_opt =
    match notification with
    | _ when server.state = State.Uninitialized ->
        Logs.warn (fun m -> m "Server is uninitialized");
        (server, None)
    | CN.Initialized ->
        (* Check the validity of the current API token here.
           We do this asynchronously, since this is purely side-effecting,
           and we don't care to percolate the monad.
        *)
        let check_token =
          Reply.later (fun send ->
              match%lwt Legacy_session.check_token server.session with
              | Ok () -> Lwt.return_unit
              | Error e ->
                  send
                    (Legacy_lsp_.notify_show_message ~kind:MessageType.Error e))
        in
        let reply =
          Reply.both check_token (Legacy_scan_helpers.refresh_rules session)
        in
        let session = Legacy_session.load_local_skipped_fingerprints session in
        let server = { server with session } in
        (server, Some reply)
    | CN.DidSaveTextDocument { textDocument = { uri }; _ } ->
        Logs.debug (fun m -> m "Scanning file %s on save" (Uri.to_string uri));
        (server, Some (Legacy_scan_helpers.scan_file session uri))
    | CN.TextDocumentDidClose { textDocument = { uri; _ } } ->
        let path = uri |> Uri.to_path |> Fpath.v in
        ( server,
          Some
            (Reply.later (fun _ ->
                 Legacy_session.remove_open_document session path)) )
    | CN.TextDocumentDidChange
        { textDocument = { uri; _ }; contentChanges = first :: _ } ->
        (* TODO: remove diagnostics if edit is in range *)
        ignore first;
        ignore uri;
        (server, None)
    | CN.TextDocumentDidOpen { textDocument = { uri; _ } } ->
        let path = uri |> Uri.to_path |> Fpath.v in
        let reply =
          Reply.later (fun send ->
              let%lwt () =
                Reply.apply send (Legacy_scan_helpers.scan_file session uri)
              in
              Legacy_session.add_open_document session path)
        in
        (server, Some reply)
    | CN.ChangeWorkspaceFolders { event = { added; removed }; _ } ->
        let session =
          let added = Conv.workspace_folders_to_paths added in
          let removed = Conv.workspace_folders_to_paths removed in
          Legacy_session.update_workspace_folders session ~added ~removed
        in
        Legacy_session.cache_workspace_targets session;
        let server = { server with session } in
        (server, Some (Legacy_scan_helpers.scan_workspace session))
    (* If files are renamed or created, update our targets *)
    | CN.DidRenameFiles _
    | CN.DidCreateFiles _ ->
        Legacy_session.cache_workspace_targets session;
        (server, None)
    | CN.DidDeleteFiles { files = paths; _ } ->
        (* This is lame, for whatever reason they chose to type uri as string here, not Uri.t *)
        Legacy_session.cache_workspace_targets session;
        let paths =
          paths
          |> List.map (fun { FileDelete.uri } ->
                 Str.string_after uri (String.length "file://") |> Fpath.v)
          (* Be careful! Because each file that DidDeleteFiles sends us might actually
             be a folder, we cannot just delete findings from those paths.
             We must check all files for which we have results, and check if they may be
             contained in the reported folder.
          *)
          |> List.concat_map (fun path ->
                 List.filter
                   (fun scanned_file -> Fpath.is_prefix path scanned_file)
                   (Legacy_session.scanned_files session))
        in
        let diagnostics =
          Legacy_diagnostics.diagnostics_of_results
            ~is_intellij:session.is_intellij [] paths
        in
        ( server,
          Some
            (Reply.later (fun send ->
                 let%lwt () =
                   Lwt_list.iter_p send (Legacy_lsp_.batch_notify diagnostics)
                 in

                 Legacy_session.remove_open_documents session paths)) )
    | CN.Exit ->
        Logs.debug (fun m -> m "Server exiting");
        ({ server with state = Legacy_lsp_.State.Stopped }, None)
    | CN.UnknownNotification { method_ = "semgrep/refreshRules"; _ } ->
        (server, Some (Legacy_scan_helpers.refresh_rules session))
    | CN.UnknownNotification { method_ = "semgrep/logout"; _ } ->
        let reply =
          if
            Semgrep_settings.save
              { (Semgrep_settings.load ()) with api_token = None }
          then
            let notif_reply =
              Reply.now
                (notify_show_message ~kind:MessageType.Info
                   "Logged out of Semgrep Code")
            in
            let refresh_reply = Legacy_scan_helpers.refresh_rules session in
            Reply.both notif_reply refresh_reply
          else
            Reply.now
              (notify_show_message ~kind:MessageType.Error "Failed to log out")
        in
        (server, Some reply)
    | CN.UnknownNotification
        { method_ = "semgrep/scanWorkspace"; params = Some json } -> (
        match session.cached_session.initialized with
        | false ->
            ( server,
              Some
                (Reply.now
                   (notify_show_message ~kind:MessageType.Warning
                      "The Semgrep Extension is still loading rules. Please \
                       wait a moment and try again.")) )
        | true ->
            let full =
              Structured.yojson_of_t json
              |> member "full" |> to_bool_option
              |> Option.value ~default:false
            in
            let session =
              {
                session with
                user_settings =
                  { session.user_settings with only_git_dirty = not full };
              }
            in
            let reply =
              Reply.later (fun send ->
                  let%lwt () =
                    if session.metrics.client_metrics.isNewAppInstall && full
                    then
                      send
                        (notify_show_message ~kind:MessageType.Info
                           "Scanning all files regardless of git status. These \
                            diagnostics will persist until a file is edited. \
                            To default to always scanning regardless of git \
                            status, please disable 'Only Git Dirty' in \
                            settings")
                    else Lwt.return_unit
                  in
                  Logs.debug (fun m -> m "Scanning workspace, full: %b" full);
                  let%lwt () =
                    Reply.apply send
                      (Legacy_scan_helpers.scan_workspace session)
                  in
                  Logs.debug (fun m -> m "Scanning workspace complete");
                  Lwt.return_unit)
            in
            ({ server with session }, Some reply))
    | CN.UnknownNotification { method_; params } ->
        (server, handle_custom_notification session method_ params)
    | _ ->
        Logs.debug (fun m ->
            m "Unhandled notification %s"
              (CN.to_jsonrpc notification |> Notification.yojson_of_t
             |> Yojson.Safe.pretty_to_string));
        (* TODO: log this to the client *)
        (server, None)
  in
  (server, reply_opt)
