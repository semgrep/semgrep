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
