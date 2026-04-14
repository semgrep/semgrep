(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
val on_request :
  Legacy_rpc_server.t ->
  Jsonrpc.Id.t ->
  'a Lsp.Client_request.t ->
  Legacy_rpc_server.t * Legacy_lsp_.Reply.t
(** [on_request request server] handles any LSP request, and returns a
    JSONRPC response, and a new server state.
  *)
