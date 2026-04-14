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
val on_notification :
  Legacy_rpc_server.t ->
  Lsp.Client_notification.t ->
  Legacy_rpc_server.t * Legacy_lsp_.Reply.t option
(** [on_notification request server] handles any LSP notification, and returns
    a new server state and possilby some notifications to send back to the client.
  *)
