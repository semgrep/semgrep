(* this can be raised when timeout_threshold is set *)
exception File_timeout of Rule_ID.t list

(* Matches many rules against one target. This function is called from
 * Test_engine.ml, Test_subcommand.ml, and of course Core_scan.ml
 * (and also Match_extract_mode.ml now).
 *
 * [dependency_match_table]: if the target had an associated lockfile, then this is a mapping from
 * supply chain rules to the dependencies they matched in that lockfile.
 * It's used to annotate pattern matches with dependency matches.
 * alt: we could do this as part of the match_hook, but it would require a couple changes.
 *      One, we'd have to make the dependency_match field of a Pattern_match.t mutable, but this minor.
 *      Two, it's currently possible for dependency matches to turn a single pattern match into *multiple* pattern matches,
 *      and match_hook cannot do this since it does not have access to the list of pattern matches produced by a rule.
 *      See: [annotate_pattern_match] in Match_dependency.ml
 *      This only happens in a very rare edge case: when we have multiple copies of a dependency in a lockfile, and they are *all*
 *      transitive, and the dependency is used in code. In this case we cannot tell which copy of the dependency is
 *      getting used, so we create a match for all of them. That's what the python code does. This is a very unusual
 *      case though, and it might be reasonable to just say this is undefined behavior and pick a random choice of match
 *
 * Return matches, errors, and match time.
 *
 * This will run the search-mode and taint-mode rules.
 * This can also raise File_timeout.
 *)
val check :
  match_hook:(string -> Pattern_match.t -> unit) ->
  timeout:float ->
  timeout_threshold:int ->
  ?dependency_match_table:Match_dependency.dependency_match_table ->
  Match_env.xconfig ->
  Rule.rules ->
  Xtarget.t ->
  Core_result.matches_single_file

(* for osemgrep interactive *)
val is_relevant_rule_for_xtarget :
  Rule.t -> Match_env.xconfig -> Xtarget.t -> bool
