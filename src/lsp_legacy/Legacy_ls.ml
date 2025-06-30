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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
open Lsp
open Types

(*****************************************************************************)
(* Message handler *)
(*****************************************************************************)

(* This module contains all networking/rpc related functionality of the
 * language server.
 *)

let on_notification = Legacy_notification_handler.on_notification
let on_request = Legacy_request_handler.on_request

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
            ~change:TextDocumentSyncKind.Incremental ~save:(`Bool true) ()))
    ~workspace:
      (ServerCapabilities.create_workspace
         ~workspaceFolders:
           (WorkspaceFoldersServerCapabilities.create ~supported:true
              ~changeNotifications:(`Bool true) ())
         ~fileOperations:
           (FileOperationOptions.create ~didCreate:reg_opts ~didRename:reg_opts
              ~didDelete:reg_opts ())
         ())
    ~hoverProvider:(`Bool true) ~codeActionProvider:(`Bool true)
    ~executeCommandProvider:
      (ExecuteCommandOptions.create
         ~commands:Legacy_execute_command.supported_commands ())
    ()

(*****************************************************************************)
(* Entry point*)
(*****************************************************************************)

(* LET'S GOOOOOO *)
let start (caps : < Legacy_session.caps ; .. >) =
  Logs.debug (fun m -> m "Starting Semgrep Language Server");
  Lwt_platform.set_engine ();
  let server =
    Legacy_rpc_server.create (caps :> Legacy_session.caps) capabilities
  in
  Legacy_rpc_server.start ~handler:{ on_notification; on_request } server
