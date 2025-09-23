(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(** Predicates for prefiltering rules.

    This module defines the basic predicates used in both file-level and
    project-level prefiltering. A predicate is a testable condition that
    can be evaluated against file content to determine if a rule might match. *)

type t =
  | String of string  (** Match exact string occurrence *)
  | Regex of Pcre2_.t  (** Match regular expression *)
[@@deriving show, eq, ord, hash, sexp_of]

val eval : t -> string -> bool
(** [eval predicate content] evaluates [predicate] against [content].
    Returns [true] if the predicate matches, [false] otherwise.

    On errors (e.g., regex matching against invalid UTF-8), this function
    returns [true] to be conservative and avoid false negatives. *)
