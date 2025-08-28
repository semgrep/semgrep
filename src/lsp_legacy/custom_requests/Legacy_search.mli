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
val start_meth : string
(** method to match on: semgrep/search *)

val mk_params :
  lang:Analyzer.t option ->
  fix:string option ->
  includes:string list ->
  excludes:string list ->
  string ->
  Jsonrpc.Structured.t

val ongoing_meth : string
(** method to match on: semgrep/searchOngoing *)

val start_search :
  Legacy_session.t ->
  Jsonrpc.Id.t ->
  Jsonrpc.Structured.t option ->
  Legacy_session.t * Legacy_lsp_.Reply.t
(** [start_search server params] will cause a search to start with the given parameters,
    storing the information of remaining rules/targets to search in the server session.
    It will then return the matches in the first file with matches.
    Will return `Assoc ["locations": `List []] when the search has concluded.
  *)

val search_next_file :
  Legacy_session.t ->
  Jsonrpc.Id.t ->
  Jsonrpc.Structured.t option ->
  Legacy_session.t * Legacy_lsp_.Reply.t
(** [search_next_file server params] is used during an ongoing search, and will
    return the matches in the first file with matches, based on the remaining
    rules/targets to search in the server session state.
    Will return `Assoc ["locations": `List []] when the search has concluded.
  *)
