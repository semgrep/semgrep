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
(* This module handles all incoming requests from the client *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

open Lsp
open Types
open Jsonrpc
module CR = Client_request

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

(* Dispatch to the various custom request handlers. *)
let handle_custom_request server (meth : string)
    (params : Jsonrpc.Structured.t option) : Yojson.Safe.t option * RPC_server.t
    =
  match
    (* Methods which can alter the server *)
    [
      (Search.start_meth, Search.start_search);
      (Search.ongoing_meth, Search.search_next_file);
    ]
    |> List.assoc_opt meth
  with
  | Some handler -> handler server params
  | None -> (
      match
        (* Methods which cannot alter the server. *)
        [
          (ShowAst.meth, ShowAst.on_request);
          (Login.meth, Login.on_request);
          (LoginStatus.meth, LoginStatus.on_request);
        ]
        |> List.assoc_opt meth
      with
      | None ->
          Logs.warn (fun m -> m "Unhandled custom request %s" meth);
          (None, server)
      | Some handler -> (handler server params, server))

let on_request (type r) (request : r CR.t) server =
  let process_result ((req : r), server) =
    (Some (CR.yojson_of_result request req), server)
  in
  Logs.debug (fun m ->
      m "Handling request:\n%s"
        (CR.to_jsonrpc_request request (`Int 0)
        |> Request.yojson_of_t |> Yojson.Safe.pretty_to_string));
  match request with
  | CR.Initialize params -> (
      try Initialize_request.on_request server params |> process_result with
      | e ->
          let backtrace = Printexc.get_backtrace () in
          Logs.err (fun m ->
              m "Error initializing server: %s" (Printexc.to_string e));
          Logs.info (fun m -> m "Backtrace: %s" backtrace);
          RPC_server.log_error_to_client "Error initializing server" e;
          let result =
            InitializeError.create ~retry:false |> InitializeError.yojson_of_t
          in
          (Some result, server))
  | _ when server.state = RPC_server.State.Uninitialized ->
      Logs.err (fun m -> m "Server not initialized, ignoring request");
      (None, server)
  | CR.CodeAction params ->
      Code_actions.on_request server params |> process_result
  | TextDocumentHover params -> Hover_request.on_request server params
  | CR.ExecuteCommand { arguments; command; _ } ->
      let args = Option.value arguments ~default:[] in
      Execute_command.handle_execute_request server command args
  | CR.UnknownRequest { meth; params } ->
      handle_custom_request server meth params
  | CR.Shutdown ->
      Logs.app (fun m -> m "Shutting down server");
      Session.save_local_skipped_fingerprints server.session;
      (None, server)
  | CR.DebugEcho params -> process_result (params, server)
  | _ ->
      Logs.debug (fun m ->
          m "Unhandled request %s"
            (CR.to_jsonrpc_request request (`Int 0)
            |> Request.yojson_of_t |> Yojson.Safe.pretty_to_string));
      (None, server)
