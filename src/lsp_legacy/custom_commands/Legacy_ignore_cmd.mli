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
(** command name *)

type t = { path : string; fingerprint : string } [@@deriving yojson]
(** which finding to ignore*)

val create : path:string -> fingerprint:string -> Lsp.Types.Command.t
(** [create ~path ~fingerprint] creates a [Command.t] command to ignore a finding at [path] with [fingerprint] *)

val command_handler :
  Legacy_session.t ->
  Yojson.Safe.t list ->
  Legacy_session.t * Legacy_lsp_.Reply.t option
(** [command_handler session params] handles the ignore finding command *)
