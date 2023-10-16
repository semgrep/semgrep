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
open Jsonrpc
module CR = Client_request

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

(* Specifically for custom requests for searching. *)
let search_handler (server : RPC_server.t) params =
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
  let runner rules =
    Scan_helpers.run_semgrep server ~rules:(Some rules) |> fst
  in
  Search.on_request runner params

(* Dispatch to the various custom request handlers. *)
let handle_custom_request server (meth : string)
    (params : Jsonrpc.Structured.t option) : Yojson.Safe.t option * RPC_server.t
    =
  match
    [
      (Search.meth, search_handler);
      (ShowAst.meth, ShowAst.on_request);
      (Login.meth, Login.on_request);
      (LoginStatus.meth, LoginStatus.on_request);
    ]
    |> List.assoc_opt meth
  with
  | None ->
      Logs.warn (fun m -> m "Unhandled custom request %s" meth);
      (None, server)
  | Some handler -> (handler server params, server)

let on_request (type r) (request : r CR.t) server =
  let process_result (r, server) =
    (Some (CR.yojson_of_result request r), server)
  in
  match request with
  | CR.Initialize params ->
      Initialize_request.on_request server params |> process_result
  | _ when server.state = RPC_server.State.Uninitialized ->
      Logs.err (fun m -> m "Server not initialized, ignoring request");
      (None, server)
  | CR.CodeAction params ->
      Code_actions.on_request server params |> process_result
  | TextDocumentHover params -> Hover_request.on_request server params
  | CR.UnknownRequest { meth; params } ->
      handle_custom_request server meth params
  | CR.Shutdown ->
      Logs.debug (fun m -> m "Shutting down server");
      (None, { server with state = RPC_server.State.Stopped })
  | CR.DebugEcho params -> process_result (params, server)
  | _ ->
      Logs.warn (fun m ->
          m "Unhandled request %s"
            (CR.to_jsonrpc_request request (`Int 0)
            |> Request.yojson_of_t |> Yojson.Safe.pretty_to_string));
      (None, server)
