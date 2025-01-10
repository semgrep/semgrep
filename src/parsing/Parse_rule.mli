(* Parse a rule file, either in YAML or JSON (or even Jsonnet) format
   depending on the filename extension.

   The parser accepts invalid rules, skips them, and returns them in
   the list of errors.
   This will not return [Error (Rule.InvalidRule _)] as the main result.
   However, this function may return the other instances of
   [Error (x : Rule.Error.t)], (e.g., [Error (Rule.InvalidYaml _)]).

   rewrite_rule_ids, if not None, provides what's needed to parse the rule
   ID 'foo' as 'path.to.foo'. This is the default behavior for 'semgrep scan'.
   See the command-line option --rewrite-rule-ids.
*)
val parse_and_filter_invalid_rules :
  ?rewrite_rule_ids:(Rule_ID.t -> Rule_ID.t) ->
  Fpath.t ->
  (Rule_error.rules_and_invalid, Rule_error.t) result

(* This is used for parsing -e/-f extended patterns in Run_semgrep.ml
 * and now also in osemgrep Config_resolver.ml.
 * This can raise Failure for spacegrep parsing errors, and returns
 * Error (Rule.InvalidRegexp _) for regexp errors.
 *)
val parse_xpattern :
  Analyzer.t -> string Rule.wrap -> (Xpattern.t, Rule_error.t) result

val parse_fake_xpattern :
  Analyzer.t -> string -> (Xpattern.t, Rule_error.t) result

(* This should be used mostly in testing code. Otherwise you should
 * use parse_and_filter_invalid_rules.
 * This function may raise (Rule.Err ....) or Assert_failure (when
 * there are invalid rules).
 *)
val parse : Fpath.t -> (Rule.rules, Rule_error.t) result

(* Internals, used by osemgrep to setup a ojsonnet import hook.
 * The filename parameter is just used in case of missing 'rules:'
 * to report the error on the first line of the file.
 *)
val parse_generic_ast :
  ?error_recovery:bool ->
  ?rewrite_rule_ids:(Rule_ID.t -> Rule_ID.t) ->
  Fpath.t ->
  AST_generic.program ->
  (Rule_error.rules_and_invalid, Rule_error.t) result
