(*
   Copyright (c) 2026 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Indexed gitignore level: builds a per-strategy hash-table index
   from parsed gitignore patterns and exposes matching as a single
   fold-friendly primitive.

   See Gitignore_level_index.ml for documentation of the matching
   strategies and index shape.

   A gitignore level is a group of patterns that has precendence applied
   to it; see Gitignore.ml for details.
*)

type t
(** An immutable, index-backed gitignore level, ready for repeated
    matching against paths. *)

val of_parsed_patterns :
  level_kind:string ->
  source_name:string ->
  Parse_gitignore.parsed_pattern list ->
  t
(** Build a level index from parsed gitignore patterns. The patterns
    are classified into matching strategies and grouped by strategy into
    hash tables for fast lookup at match time. *)

val level : t -> Gitignore.level
(** Diagnostics / pretty-printing accessor for the underlying level. *)

val select_level : t -> Ppath.t -> Gitignore.selection_event list
(** Match a single level against a path. Returns selection events in
    reverse pattern order — the event at the head corresponds to the
    last-applied pattern, which matches the convention of
    [Gitignore.selection_event] lists. *)

val select_level_naive : t -> Ppath.t -> Gitignore.selection_event list
(** Reference implementation of [select_level] that runs every pattern's
    PCRE matcher directly, bypassing the strategy index. Exposed for
    equivalence tests — not intended as a public matching API. *)

(*****************************************************************************)
(* Strategy classifier — exposed only for tests. *)
(*****************************************************************************)

type strategy =
  (* The pattern matches if and only if the basename of the file matches the literal string. *)
  | Basename_literal of { basename : string; dir_only : bool }
  (* The pattern matches if and only if the entire path matches the literal string. *)
  | Literal of { path : string; dir_only : bool }
  (* The pattern matches if and only if the path's extension matches the literal string. *)
  | Extension of { ext : string; dir_only : bool }
  (* The pattern MAY match if the path's extension matches the literal string, but additional checks are required. *)
  | Required_extension of { ext : string }
  (* No other strategy covers the pattern, and therefore regex is required. *)
  | Regex

val classify : Glob.Pattern.t -> strategy
(** Classify an absolute glob pattern into a matching strategy.
    Exposed for equivalence tests — not intended as a public API. *)
