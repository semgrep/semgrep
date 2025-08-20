(** Predicates for prefiltering rules.

    This module defines the basic predicates used in both file-level and
    project-level prefiltering. A predicate is a testable condition that
    can be evaluated against file content to determine if a rule might match. *)

type t =
  | String of string  (** Match exact string occurrence *)
  | Regex of Pcre2_.t  (** Match regular expression *)
[@@deriving show, eq, ord, hash]

val eval : t -> string -> bool
(** [eval predicate content] evaluates [predicate] against [content].
    Returns [true] if the predicate matches, [false] otherwise.

    On errors (e.g., regex matching against invalid UTF-8), this function
    returns [true] to be conservative and avoid false negatives. *)
