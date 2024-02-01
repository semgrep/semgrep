(* Dependency matching for Semgrep Supply Chain. *)

(* Entry point for generating lockfile-only findings.
   Takes a rule with a dependency formula and a lockfile target, and produces
   full [Pattern_match.t]s for each dependency in the lockfile that matches the dependency formula.
*)
val check_rule :
  Rule.rule ->
  Lockfile_target.t ->
  Rule.dependency_formula ->
  Core_profiling.rule_profiling Core_result.match_result

(* Entry point for generating "reachable" findings.
   Takes a list of dependency matches from a lockfile and a pattern match
   The dependency matches should be from the lockfile that was associated to the code file that pattern match was generated from.
   The list of dependency matches should never be empty, though if it is, it will produce an empty list of pattern matches.

   If there is one dependency match (say [dm]), then this will produce a list with one pattern match, which should be the same as the input
   pattern match, but with the [dependency] field set to [CodeAndLockfileMatch dm].

   If there are multiple dependency matches, then there are two possibilities:
   1. We have n matches of the same dependency, and *all* of them are transitive, *and* we are using the dependency in code (this is the input pattern match)
      This case should be quite rare, but if it happens, we have no way to tell which transitive copy of the dependency is being used. It is package-manager
      dependent and probably even project dependent, so we make the choice to produce n pattern matches, each annotated with one of the transitive dependencies,
      to represent each possibility. This is what the python code does. It would probably be reasonable to say this case is undefined, and randomly pick
      one copy of the dependency to annotate the pattern match with.
    2. We have n matches of the same dependency, and one is direct, whie (n - 1) are transitive. In this case we know that the code must be using the direct one,
       so we produce *one* pattern match, annotated with the dependency match direct copy.
*)

val annotate_pattern_match :
  Pattern_match.dependency_match list option ->
  Pattern_match.t ->
  Pattern_match.t list
