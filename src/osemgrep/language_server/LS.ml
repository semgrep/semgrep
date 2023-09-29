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
open Jsonrpc
open Lsp
open Types
open Yojson.Safe.Util
open RPCServer
module CN = Client_notification
module CR = Client_request
module Conv = Convert_utils
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* TODO: Explain what Semgrep LS is here *)

(*****************************************************************************)
(* Semgrep helpers *)
(*****************************************************************************)

(* Relevant here means any matches we actually care about showing the user.
    This means like some matches, such as those that appear in committed
    files/lines, will be filtered out*)

(** This is the entry point for scanning, returns /relevant/ matches, and all files scanned*)
let run_semgrep ?(targets = None) ?(rules = None) ({ session; _ } : RPCServer.t)
    =
  let rules = Option.value ~default:session.cached_session.rules rules in
  Logs.app (fun m -> m "Running Semgrep with %d rules" (List.length rules));
  (* !!!Dispatch to the Semgrep engine!!! *)
  let res =
    let targets = Option.value ~default:(Session.targets session) targets in
    let runner_conf = Session.runner_conf session in
    let scan_func =
      Core_runner.mk_scan_func_for_osemgrep Core_scan.scan_with_exn_handler
    in
    scan_func ~respect_git_ignore:true ~file_match_results_hook:None runner_conf
      rules [] targets
    |> Core_runner.create_core_result rules
  in
  (* Collect results. *)
  let scanned = res.scanned |> Set_.elements in
  Logs.app (fun m -> m "Scanned %d files" (List.length scanned));
  let matches =
    let only_git_dirty = session.user_settings.only_git_dirty in
    Processed_run.of_matches ~only_git_dirty res
  in
  Logs.app (fun m -> m "Found %d matches" (List.length matches));
  (matches, scanned)

(** Scan all folders in the workspace *)
let scan_workspace server =
  let token =
    create_progress server "Semgrep Scan in Progress" "Scanning Workspace"
  in
  let results, files = run_semgrep server in
  Session.record_results server.session results files;
  (* LSP expects empty diagnostics to clear problems *)
  let diagnostics =
    let files = Session.scanned_files server.session in
    Diagnostics.diagnostics_of_results ~is_intellij:server.session.is_intellij
      results files
  in
  end_progress server token;
  batch_notify server diagnostics

(** Scan a single file. Passing [content] will write it to a temp file,
  and scan that temp file, then return results as if [uri] was scanned *)
let scan_file ?(content = None) server uri =
  let file_path = Uri.to_path uri in
  let file = Fpath.v file_path in
  let targets =
    let session_targets = Session.targets server.session in
    let targets =
      match content with
      | None -> [ file ]
      | Some content ->
          let name = Fpath.basename file in
          let ext = Fpath.get_ext file in
          let tmp_file = Common.new_temp_file name ext in
          Common.write_file tmp_file content;
          [ Fpath.v tmp_file ]
    in
    if List.mem file session_targets then Some targets else Some []
  in
  let matches =
    let cli_matches, _ = run_semgrep ~targets server in
    Common.map
      (fun (m : Out.cli_match) -> { m with path = file_path })
      cli_matches
  in
  let files = [ file ] in
  Session.record_results server.session matches files;
  let diagnostics =
    Diagnostics.diagnostics_of_results ~is_intellij:server.session.is_intellij
      matches files
  in
  batch_notify server diagnostics

let refresh_rules server =
  let token = create_progress server "Semgrep" "Refreshing Rules" in
  Lwt.async (fun () ->
      let%lwt () = Session.cache_session server.session in
      end_progress server token;
      scan_workspace server;
      Lwt.return_unit)

(*****************************************************************************)
(* Notifications *)
(*****************************************************************************)

let on_notification notification server =
  let server =
    match notification with
    | _ when server.state = State.Uninitialized -> server
    | CN.Initialized ->
        Logs.app (fun m -> m "Server initialized");
        refresh_rules server;
        server
    | CN.TextDocumentDidChange
        { textDocument = { uri; _ }; contentChanges = first :: _ } ->
        Logs.app (fun m -> m "Scanning file %s on change " (Uri.to_string uri));
        let content = Some first.text in
        scan_file ~content server uri;
        server
    | CN.DidSaveTextDocument { textDocument = { uri }; _ } ->
        Logs.app (fun m -> m "Scanning file %s on save" (Uri.to_string uri));
        scan_file server uri;
        server
    | CN.TextDocumentDidOpen { textDocument = { uri; _ } } ->
        let path = uri |> Uri.to_path |> Fpath.v in
        let prev_scan = Session.previous_scan_of_file server.session path in
        (* We usually scan every file on startup, so let's only rescan an opened
            file if there weren't previous results *)
        if Option.is_some prev_scan then
          Logs.app (fun m ->
              m "File %s already scanned, not rescanning" (Uri.to_string uri))
        else (
          Logs.app (fun m -> m "Scanning file %s on open" (Uri.to_string uri));
          scan_file server uri);
        server
    | CN.ChangeWorkspaceFolders { event = { added; removed }; _ } ->
        let added = Conv.workspace_folders_to_paths added in
        let removed = Conv.workspace_folders_to_paths removed in
        let session =
          Session.update_workspace_folders server.session ~added ~removed
        in
        let server = { server with session } in
        scan_workspace server;
        server
    | CN.DidDeleteFiles { files; _ } ->
        (* This is lame, for whatever reason they chose to type uri as string here, not Uri.t *)
        let files =
          Common.map
            (fun { FileDelete.uri } ->
              Str.string_after uri (String.length "file://") |> Fpath.v)
            files
        in
        let diagnostics =
          Diagnostics.diagnostics_of_results
            ~is_intellij:server.session.is_intellij [] files
        in
        batch_notify server diagnostics;
        server
    | CN.Exit ->
        Logs.app (fun m -> m "Server exiting");
        { server with state = State.Stopped }
    | CN.UnknownNotification { method_ = "semgrep/refreshRules"; _ }
    | CN.UnknownNotification { method_ = "semgrep/loginFinish"; _ }
    | CN.UnknownNotification { method_ = "semgrep/logout"; _ } ->
        refresh_rules server;
        server
    | CN.UnknownNotification
        { method_ = "semgrep/scanWorkspace"; params = Some json } ->
        let json = Structured.yojson_of_t json in
        let full =
          json |> member "full" |> to_bool_option |> Option.value ~default:false
        in
        Logs.app (fun m -> m "Scanning workspace, full: %b" full);
        let session =
          {
            server.session with
            user_settings =
              { server.session.user_settings with only_git_dirty = not full };
          }
        in
        let _server = { server with session } in
        scan_workspace _server;
        Logs.app (fun m -> m "Scanning workspace complete");
        server
    | _ ->
        Logs.warn (fun m ->
            m "Unhandled notification %s"
              (CN.to_jsonrpc notification |> Notification.yojson_of_t
             |> Yojson.Safe.pretty_to_string));
        server
  in
  server

(*****************************************************************************)
(* Requests *)
(*****************************************************************************)

let handle_custom_request server (meth : string)
    (params : Jsonrpc.Structured.t option) : Yojson.Safe.t option * t =
  let search_handler params =
    let server =
      let session = server.session in
      let session =
        {
          session with
          user_settings = { session.user_settings with only_git_dirty = false };
        }
      in
      { server with session }
    in
    let runner rules = run_semgrep server ~rules:(Some rules) |> fst in
    Search.on_request runner params
  in
  match
    [ (Search.meth, search_handler); (ShowAst.meth, ShowAst.on_request) ]
    |> List.assoc_opt meth
  with
  | None ->
      Logs.warn (fun m -> m "Unhandled custom request %s" meth);
      (None, server)
  | Some handler -> (handler params, server)

let on_request (type r) (request : r CR.t) server =
  let process_result (r, server) =
    (Some (CR.yojson_of_result request r), server)
  in
  match request with
  | CR.Initialize params ->
      InitializeRequest.on_request server params |> process_result
  | _ when server.state = State.Uninitialized ->
      Logs.err (fun m -> m "Server not initialized, ignoring request");
      (None, server)
  | CR.CodeAction params ->
      CodeActions.on_request server params |> process_result
  | TextDocumentHover params -> HoverRequest.on_request server params
  | CR.UnknownRequest { meth; params } ->
      handle_custom_request server meth params
  | CR.Shutdown ->
      Logs.app (fun m -> m "Shutting down server");
      (None, { server with state = State.Stopped })
  | CR.DebugEcho params -> process_result (params, server)
  | _ ->
      Logs.warn (fun m ->
          m "Unhandled request %s"
            (CR.to_jsonrpc_request request (`Int 0)
            |> Request.yojson_of_t |> Yojson.Safe.pretty_to_string));
      (None, server)

(*****************************************************************************)
(* Message handler *)
(*****************************************************************************)

(* This module contains all networking/rpc related functionality of the
 * language server.
 *)

module MessageHandler = struct
  let on_notification = on_notification
  let on_request = on_request

  (* See: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification *)

  (** Everything this server supports from the LSP *)
  let capabilities =
    let fil =
      FileOperationFilter.create
        ~pattern:(FileOperationPattern.create ~glob:"**/*" ())
        ()
    in
    let reg_opts = FileOperationRegistrationOptions.create ~filters:[ fil ] in
    ServerCapabilities.create
      ~textDocumentSync:
        (`TextDocumentSyncOptions
          (TextDocumentSyncOptions.create ~openClose:true
             ~change:TextDocumentSyncKind.Full ~save:(`Bool true) ()))
      ~workspace:
        (ServerCapabilities.create_workspace
           ~workspaceFolders:
             (WorkspaceFoldersServerCapabilities.create ~supported:true
                ~changeNotifications:(`Bool true) ())
           ~fileOperations:
             (FileOperationOptions.create ~didCreate:reg_opts
                ~didRename:reg_opts ~didDelete:reg_opts ())
           ())
      ~hoverProvider:(`Bool true) ~codeActionProvider:(`Bool true) ()
end

module LanguageServer = RPCServer.Make (MessageHandler)

(*****************************************************************************)
(* Entry point*)
(*****************************************************************************)

(* LET'S GOOOOOO *)
let start () =
  Logs.app (fun m -> m "Starting Semgrep Language Server");
  let server = LanguageServer.create () in
  LanguageServer.start server;
  exit 0
