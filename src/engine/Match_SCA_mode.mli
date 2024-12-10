(* Dependency matching for Semgrep Supply Chain. *)

(* Entry point for generating lockfile-only matches.
   Takes a rule with a dependency formula and a lockfile target, and produces
   full [Core_match.t]s for each dependency in the lockfile that matches the
   dependency formula.

   When the input rule also a code pattern, if it separately produced a code
   match annotated with a dependency match, from the input lockfile, then any
   lockfile-only findings produced by [check_rule] in this case should later be
   removed from the final match output. This is because a lockfile-only finding
   from such a rule is meant to indicate it had *no* code matches.
   The reason this is done in post processing is to avoid introducing data
   dependencies between matches, which would impede parallelism.
*)
val check_rule :
  Rule.t ->
  Lockfile_xtarget.t ->
  Rule.sca_dependency_formula ->
  Core_profiling.rule_profiling Core_result.match_result

(* Entry point for generating "reachable" matches,
   Takes a list of dependency matches from a lockfile and a pattern match
   The dependency matches should be from the lockfile that was associated to the
   code file that pattern match was generated from.
   The list of dependency matches should never be empty, though if it is, it
   will produce an empty list of pattern matches.

   If there is one dependency match (say [dm]), then this will produce a list
   with one pattern match, which should be the same as the input pattern match,
   but with the [dependency] field set to [CodeAndLockfileMatch dm].

   If there are multiple dependency matches, then there are two possibilities:
   1. We have n matches of the same dependency, and *all* of them are
      transitive, *and* we are using the dependency in code (this is the input
      pattern match)
      This case should be quite rare, but if it happens, we have no way to tell
      which transitive copy of the dependency is being used. It is
      package-manager dependent and probably even project dependent, so we make
      the choice to produce n pattern matches, each annotated with one of the
      transitive dependencies, to represent each possibility. This is what the
      python code does. It would probably be reasonable to say this case is
      undefined, and randomly pick one copy of the dependency to annotate the
      pattern match with.
    2. We have n matches of the same dependency, and one is direct, while
       (n - 1) are transitive. In this case we know that the code must be using
       the direct one, so we produce *one* pattern match, annotated with the
       dependency match direct copy.
*)

val annotate_pattern_match :
  SCA_match.t list option -> Core_match.t -> Core_match.t list

(* Used in [check_rule]
   Takes a lockfile target and a list rules, and associates each rule
   to [None] if it has no dependency formula, and
   [Some dep_matches] if it has a dependency formula, where [dep_matches]
   is a list of all dependencies in the lockfile that matched the dependency
   formula.
*)
val match_all_dependencies :
  Lockfile_xtarget.t ->
  Rule.rule list ->
  (Rule.rule * SCA_match.t list option) list

(* Used in match_rules.ml to pass down dependency match information to the
   pattern-match-generating functions *)
type dependency_match_table = (Rule_ID.t, SCA_match.t list) Hashtbl.t
