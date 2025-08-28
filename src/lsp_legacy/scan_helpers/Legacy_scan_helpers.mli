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
val run_core_search :
  Match_env.xconfig ->
  Rule.search_rule ->
  Fpath.t ->
  Core_result.processed_match list option
(** [run_core_search] runs a search intended for the /semgrep/search IDE
    search command, by hooking lower into the Match_search_mode matching
    process, bypassing the CLI.
  *)

val scan_workspace : Legacy_session.t -> Legacy_lsp_.Reply.t
(** [scan_workspace server] scans the workspace of the given session. *)

val scan_open_documents : Legacy_session.t -> Legacy_lsp_.Reply.t
(** [scan_open_documents server] scans the open documents of the given session. *)

val scan_file : Legacy_session.t -> Lsp__Uri0.t -> Legacy_lsp_.Reply.t
(** [scan_file server] scans the given file. If [content] is provided, it will
  * be used as the content of the file. Otherwise, the content will be read
  * from the file system.
  *)

val refresh_rules : Legacy_session.t -> Legacy_lsp_.Reply.t
(** [refresh_rules server] refreshes the rules of the given session. *)
