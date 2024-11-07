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
open Lsp_
open Types
open Jsonrpc
module CR = Client_request

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

(* Dispatch to the various custom request handlers. *)
let handle_custom_request session (meth : string) (params : Structured.t option)
    : Session.t * Yojson.Safe.t option =
  match
    (* Methods which can alter the server *)
    [
      (Search.start_meth, Search.start_search);
      (Search.ongoing_meth, Search.search_next_file);
    ]
    |> List.assoc_opt meth
  with
  | Some handler -> handler session params
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
          (* TODO: Notify client *)
          Logs.warn (fun m -> m "Unhandled custom request %s" meth);
          (session, None)
      | Some handler -> (session, handler session params))

let on_request (type r) server (req_id : Id.t) (request : r CR.t) :
    RPC_server.t * Reply.t =
  let process_result ((response : r), server) =
    (server, Reply.now (respond req_id request response))
  in
  let process_json_result ((response_json : Yojson.Safe.t option), server) =
    let reply =
      match response_json with
      | None -> Reply.empty
      | Some response_json -> Reply.now (respond_json req_id response_json)
    in
    (server, reply)
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
          let reply =
            Reply.later (fun send ->
                let%lwt () =
                  send (Lsp_.log_error_to_client "Error initializing server" e)
                in
                let result =
                  InitializeError.create ~retry:false
                  |> InitializeError.yojson_of_t |> respond_json req_id
                in
                send result)
          in
          (server, reply))
  | _ when server.state = State.Uninitialized ->
      Logs.err (fun m -> m "Server not initialized, ignoring request");
      (* Explicitly don't respond *)
      (server, Reply.empty)
  | CR.CodeAction params ->
      Code_actions.on_request server params |> process_result
  | TextDocumentHover params ->
      Hover_request.on_request server params |> process_json_result
  | CR.ExecuteCommand { arguments; command; _ } ->
      let args = Option.value arguments ~default:[] in
      let session, reply_opt =
        Execute_command.handle_execute_request server.session command args
      in
      ({ server with session }, Option.value reply_opt ~default:Reply.empty)
  | CR.UnknownRequest { meth; params } ->
      (* Could be handled better but :shrug: *)
      if meth = Login.meth && Semgrep_login.is_logged_in_weak () then
        let reply =
          Reply.now
            (Lsp_.notify_show_message ~kind:MessageType.Info
               "Already logged in to Semgrep Code")
        in
        (server, reply)
      else
        let session, yojson_opt =
          handle_custom_request server.session meth params
        in
        process_json_result (yojson_opt, { server with session })
  | CR.Shutdown ->
      Logs.app (fun m -> m "Shutting down server");
      Session.save_local_skipped_fingerprints server.session;
      (server, Reply.empty)
  | CR.DebugEcho params -> process_result (params, server)
  | _ ->
      Logs.debug (fun m ->
          m "Unhandled request %s"
            (CR.to_jsonrpc_request request (`Int 0)
            |> Request.yojson_of_t |> Yojson.Safe.pretty_to_string));
      (* TODO: error response, log to client *)
      (server, Reply.empty)
