(* Parse a rule file, either in JSON or YAML (or even JSONNET) format
 * depending on the filename extension.
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
 * This function may raise (Rule.Err ....) or Assert_failure (when
 * there are invalid rules).
 *)
val parse : Common.filename -> Rule.rules

(* this can be used for parsing -e/-f extended patterns in Run_semgrep.ml
 * and now also in osemgrep Config_resolver.ml.
 *)
val parse_xpattern : Xlang.t -> string Rule.wrap -> Xpattern.t

(* ex: foo.yaml, foo.yml, but not foo.test.yaml.
 *
 * Note that even if parse() above accepts JSON (and Jsonnet) files,
 * foo.json (and foo.jsonnet) are currently not considered
 * valid_rule_filename.
 *
 * This function is currently used for osemgrep, to get all
 * the valid rule files when using --config <DIR>,
 * and also in Test_engine.ml.
 *)
val is_valid_rule_filename : Common.filename -> bool
