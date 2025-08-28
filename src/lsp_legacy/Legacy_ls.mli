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
val capabilities : Lsp.Types.ServerCapabilities.t
(** The capabilities of the server. This is used to inform the client of what
    the server can do. Exposed for testing *)

val start : < Legacy_session.caps ; .. > -> unit Lwt.t
(** Entry point of the language server. This will start the server, and
    communicate over stdin/out using the Language Server Protocol *)
