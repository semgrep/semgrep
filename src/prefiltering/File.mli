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
(** File-level prefilters for rules.

    A prefilter is a predicate, generated from a rule, which can be tested to
    determine if that rule *may* match a file. That is, the prefilter
    accepting a file is a necessary but not sufficient condition for the rule
    to match the file.

    Prefilters are used to quickly eliminate files that definitely won't match
    a rule without running the full matching engine. *)

type t [@@deriving show]
(** An opaque prefilter for file-level matching *)

type predicate = Predicate.t =
  | String of string  (** Match exact string occurrence *)
  | Regex of Pcre2_.t  (** Match regular expression *)
[@@deriving show, eq, ord, hash]

val of_rule : ?interfile:bool -> Rule.t -> t option
(** [of_rule ~interfile rule] analyzes [rule] and returns optionally a prefilter.

    The returned prefilter relies on a formula of regex that we try to extract
    from the rule. For example, with:

    {v
    pattern-either:
    - pattern: foo()
    - pattern: bar()
    v}

    we will extract a condition to the effect of [Or [/foo/; /bar/]], and the
    returned function will check whether this condition matches the content
    of a file.

    This function returns [None] when it was not able to extract a condition,
    which can happen because the formula is too general (e.g., [pattern:
    $XX($YY)]). In that case, [None] is really the same as returning a
    trivially true condition (which means we should analyze the target
    file).

    When [interfile] is [true], enables interfile-compatible prefiltering.
    This means the prefilter must account for naming/type information we obtain
    only in a interfile scan. For instance, the pattern [($X : Foo)] might
    match in a file which does not contain [Foo] because the lexically present
    type could be a subtype of [Foo] (but only when interfile naming is used).
    *)

val check : t -> string -> bool
(** [check prefilter content] tests if [prefilter] matches [content].
    Returns [true] if the prefilter matches (rule should be considered),
    [false] if it doesn't match (rule can be skipped). *)

val check_many : t list -> string -> bool list
(** [check_many prefilters content] tests each prefilter in [prefilters]
    against [content]. Returns a list of results in the same order. *)

val to_formula : t -> predicate Formula.t
(** [to_formula prefilter] converts [prefilter] to its underlying formula
    representation. This exposes the logical structure for advanced use cases. *)

val of_formula : predicate Formula.t -> t
(** [of_formula formula] creates a prefilter from a [formula]. *)

val to_semgrep_formula : t -> Semgrep_prefilter_t.formula
(** [to_semgrep_formula prefilter] converts [prefilter] to the external
    ATD representation for serialization and external tools. *)

(** {2 Internal functions} *)

module Private : sig
  val of_formula :
    interfile:bool -> analyzer:Analyzer.t -> Rule.formula -> t option
  (** WARNING: Internal for testing. Do not use directly. *)
end
