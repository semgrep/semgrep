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
val supported_commands : string list
(** [supported_commands] is a list of all the LSP commands that are supported by the server. *)

val handle_execute_request :
  Legacy_session.t ->
  string ->
  Yojson.Safe.t list ->
  Legacy_session.t * Legacy_lsp_.Reply.t option
(** [handle_execute_request server command args] handles an LSP Command, and returns a response option, and updated server.
    LSP Commands are usually triggered through code actions, such as autofix, or ignoring findings. *)
