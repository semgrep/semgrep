(*
   Copyright (c) 2024-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
val meth : string
(** method to match on: [semgrep/loginFinish] *)

val mk_login_response :
  Semgrep_output_v1_t.deployment_config -> Auth.token -> Yojson.Safe.t

val on_request :
  Legacy_session.t ->
  Jsonrpc.Id.t ->
  Jsonrpc.Structured.t option ->
  Legacy_session.t * Legacy_lsp_.Reply.t
(** [on_request] will start an asynchronous job to process the
    session information and complete the authentication process for login
    *)
