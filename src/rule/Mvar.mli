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
(* a metavariable name (e.g. "$FOO") *)
type t = string [@@deriving eq, hash, ord, sexp, show]

(* return whether a string could be a metavariable name (e.g., "$FOO", but not
 * "FOO"). This mostly check for the regexp $[A-Z_][A-Z_0-9]* but
 * also handles special variables like $_GET in PHP which are actually
 * not metavariables.
 *)
val is_metavar_name : t -> bool

(* example: "$...FOO" is a metavariable ellipsis *)
val is_metavar_ellipsis : t -> bool

(* metavariables like $_ *)
val is_anonymous_metavar : t -> bool

(* example: "$1" *)
val is_metavar_for_capture_group : string -> bool

(* ??? *)
val mvars_of_regexp_string : string -> t list
