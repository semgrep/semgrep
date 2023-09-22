(*
   Rule parsing
*)

(*
   For rules that come from a local file, this is the path to the file
   containing the rule. It's used to create a dotted rule identifier
   such as 'stuff.rules.foo' for a rule declared as 'foo' in the file
   'stuff/rules.yml'.
*)
type origin = Local_file of Fpath.t | Other_origin [@@deriving show]

(* Parse a rule file, either in YAML or JSON (or even Jsonnet) format
   depending on the filename extension.

   The parser accepts invalid rules, skips them, and returns them in
   the list of errors.
   This will not raise Rule.Err (Rule.InvalidRule ...) exceptions.

   However, this function may raise the other (Rule.Err ...) exns
   (e.g., Rule.InvalidYaml).

   rewrite_rule_ids, if not None, provides what's needed to parse the rule
   ID 'foo' as 'path.to.foo'. This is the default behavior for 'semgrep scan'.
   See the command-line option --rewrite-rule-ids.
*)
val parse_and_filter_invalid_rules :
  rewrite_rule_ids:origin option ->
  Fpath.t ->
  Rule.rules * Rule.invalid_rule_error list

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
val is_valid_rule_filename : Fpath.t -> bool

(* This is used for parsing -e/-f extended patterns in Run_semgrep.ml
 * and now also in osemgrep Config_resolver.ml.
 * This can raise Failure for spacegrep parsing errors, and
 * Rule.InvalidRegexp for regexp errors.
 * For lang.t, we now parse the pattern lazily, so if you want
 * to get the possible Rule.InvalidPattern exn, you need to
 * force evaluate XPattern.Sem (lpat_lazy, _).
 *)
val parse_xpattern : Xlang.t -> string Rule.wrap -> Xpattern.t

(* This should be used mostly in testing code. Otherwise you should
 * use parse_and_filter_invalid_rules.
 * This function may raise (Rule.Err ....) or Assert_failure (when
 * there are invalid rules).
 *)
val parse : rewrite_rule_ids:origin option -> Fpath.t -> Rule.rules

(* Internals, used by osemgrep to setup a ojsonnet import hook.
 * The filename parameter is just used in case of missing 'rules:'
 * to report the error on the first line of the file.
 *)
val parse_generic_ast :
  ?error_recovery:bool ->
  rewrite_rule_ids:origin option ->
  Fpath.t ->
  AST_generic.program ->
  Rule.rules * Rule.invalid_rule_error list
