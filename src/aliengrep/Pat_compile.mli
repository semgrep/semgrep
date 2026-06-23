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
(*
   Compile a pattern into a regexp.
*)

type metavariable_kind =
  | Metavariable
  | Metavariable_ellipsis (* regular or long *)
[@@deriving show, eq]

type metavariable = {
  kind : metavariable_kind;
  bare_name : string; (* 'X', not '$X', not '$...X' *)
}
[@@deriving show, eq]

type t = private {
  pcre : Pcre2_.t;
  metavariable_groups : (int * metavariable) list;
}
[@@deriving show, eq]

(* Shortcut for all parsing + compilation *)
val from_string : Conf.t -> string -> t

(* Convert a metavariable to concrete semgrep syntax e.g. '$X' or '$...X' *)
val string_of_metavariable : metavariable -> string
