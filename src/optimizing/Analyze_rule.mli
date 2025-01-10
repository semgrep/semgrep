(* A prefilter is a pair containing a function
 * that when applied to the content of a file will return whether
 * or not we should process the file. False means that we can
 * skip the file (or more accurately, it means we can skip this rule
 * for this file, and if there are no more rules left for this target file,
 * we will entirely skip the file because we now parse files lazily).
 *)
type prefilter = Semgrep_prefilter_t.formula * (string -> bool)

(* Computing the `regex_prefilter_of_rule` can be pretty heavy in
   Previously, we created hmemo at the toplevel of this file. This caused
      problems with tests that ended up reusing that table which were very
      confusing to debug. To prevent that from happening again, the table is
      now passed to this function. For convenience you can also choose not to
       memoize. *)
type prefilter_cache = (Rule_ID.t, prefilter option) Hashtbl.t

(* This function analyzes a rule and returns optionaly a prefilter.
 *
 * The return prefilter relies on a formula of
 * regexps that we try to extract from the rule. For example, with:
 *   pattern-either: foo()
 *   pattern-either: bar()
 *
 * we will extract the 'foo|bar' regexp, and the returned function
 * will check whether this regexp matches the content of a file.
 *
 * This function returns None when it was not able to extract
 * a formula of regexps (it bailed out), which can happen because
 * the formula is too general (e.g., pattern: $XX($YY)).
 * In that case, None is really the same than returning a function
 * that always return true (which means we should analyze the target file).
 *
 * Note that this function use Common.memoized on the rule id
 *)
val regexp_prefilter_of_rule :
  cache:prefilter_cache option -> Rule.t -> prefilter option

(* WARNING: internal, do not use directly, not memoized
 *
 * The 'analyzer' is used when determining whether we can use a `metavariable-regex`
 * for pre-filtering. When the target-analyzer is Spacegrep/Aliengrep, then we
 * can always use `metavariable-regex`es for pre-filtering, because there is no
 * constant folding in generic mode. But, when running semantic analysis for a
 * specific language, we only use a `metavariable-regex` for pre-filtering
 * if the metavariable meets certain conditions. Note that, in general, if we're
 * looking for a string matching a certain regex, that regex may not match the
 * source file, but there could be a string expression that would match it at
 * runtime, and that can be known to Semgrep statically via constant folding.
 * *)
val regexp_prefilter_of_formula :
  analyzer:Analyzer.t -> Rule.formula -> prefilter option

(* For external tools like Semgrep query console to be able to
 * also prune certain rules/files.
 *)
val prefilter_formula_of_prefilter : prefilter -> Semgrep_prefilter_t.formula
