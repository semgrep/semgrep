(** Low-level prefilter generation logic.

    This module contains the core analysis algorithms for extracting prefilter
    conditions from Semgrep rules. It is not intended for general use; if
    you're trying to use a prefilter go through [File] (or the modules in Pro's
    prefiltering library) instead.

    The analysis works by:
    1. Extracting patterns and conditions from rule formulas
    2. Simplifying patterns to identify strings and regexes
    3. Converting to logical formulas of textual predicates
    4. Handling special cases like taint rules *)

val generate_prefilter :
  interfile:bool -> Rule.t -> Predicate.t Formula.t option
(** [generate_prefilter ~interfile rule] analyzes [rule] and extracts
    a logical formula of predicates that must be satisfied for the rule
    to potentially match. Returns [None] if no meaningful prefilter can
    be generated.

    When [interfile] is [true], enables interfile-compatible prefiltering.
    This means the prefilter must account for naming/type information we obtain
    only in a interfile scan. For instance, the pattern [($X : Foo)] might
    match in a file which does not contain [Foo] because the lexically present
    type could be a subtype of [Foo] (but only when interfile naming is used).
    *)

val generate_prefilter_from_formula :
  interfile:bool ->
  analyzer:Analyzer.t ->
  Rule.formula ->
  Predicate.t Formula.t option
(** [generate_prefilter_from_formula ~interfile ~analyzer formula]
    generates a prefilter from a rule formula directly. If you have the full
    rule, prefer [generate_prefilter] instead. *)
