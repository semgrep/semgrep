(* Parse a rule file.
 *
 * The parser accepts invalid rules, skips them, and returns them in
 * the list of errors.
 * This will not raise Rule.Err (Rule.InvalidRule ...) exceptions.
 *
 * However, this function may raise the other (Rule.Err ...) exns (e.g., Rule.InvalidYaml).
 *)
val parse_and_filter_invalid_rules :
  Common.filename -> Rule.rules * Rule.invalid_rule_error list

(* This should be used mostly in testing code. Otherwise you should
 * use parse_and_filter_invalid_rules.
 * This function may raise (Rule.Err ....)
 *)
val parse : Common.filename -> Rule.rules

(* this can be used for parsing -e/-f extended patterns in Run_semgrep.ml
 * and now also in osemgrep Config_resolver.ml.
 *)
val parse_xpattern : Xlang.t -> string Rule.wrap -> Xpattern.t

(* ex: foo.yaml, foo.yml, but not foo.test.yaml *)
val is_valid_rule_filename : Common.filename -> bool
