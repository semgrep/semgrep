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
  Lsp.Types.InitializeParams.t ->
  Legacy_rpc_server.t * Lsp.Types.InitializeResult.t
(** [on_request server params] is the result of handling the initialize request
  * [params] on the server [server].
  *
  * See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
  *)
