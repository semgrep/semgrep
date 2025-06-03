type prefilter [@@deriving show]
(** A prefilter is a predicate, generated from a rule, which can be tested to
    determine if that rule *may* match a file. That is, the prefilter accepting a
    file is a necessary but not sufficient condition for the rule to match the file. *)

val prefilter_of_rule : interfile:bool -> Rule.t -> prefilter option
(** This function analyzes a rule and returns optionaly a prefilter.

    The return prefilter relies on a formula of regex that we try to extract
    from the rule. For example, with:
      pattern-either:
      - pattern: foo()
      - pattern: bar()

    we will extract the condition [Or [Regex foo, Regex bar regexp, and the
    returned function will check whether this formula matches the content of a
    file.

    This function returns None when it was not able to extract a formula (it
    bailed out), which can happen because the formula is too general (e.g.,
    pattern: {v $XX($YY) }).

    In that case, [None] is really the same as returning a function that always
    return true (which means we should analyze the target file).  *)

val check_prefilter : prefilter -> string -> bool

(* For external tools like Semgrep query console to be able to
 * also prune certain rules/files.
 *)
val prefilter_formula_of_prefilter : prefilter -> Semgrep_prefilter_t.formula

(** WARNING: Internal for testing. Do not use directly. *)
module Private : sig
  val prefilter_of_formula :
    interfile:bool -> analyzer:Analyzer.t -> Rule.formula -> prefilter option
end
