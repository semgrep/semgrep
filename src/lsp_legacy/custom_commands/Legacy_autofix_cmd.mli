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
val command : string

val create : unit -> Lsp.Types.Command.t
(** [create ()] creates a new command that will let the LS know a fix was applied*)

val command_handler :
  Legacy_session.t -> 'a -> Legacy_session.t * Legacy_lsp_.Reply.t option
(** [command_handler session state] records the fact that a fix was applied in the state *)
