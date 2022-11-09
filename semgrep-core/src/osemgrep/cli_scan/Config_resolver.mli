type rules_and_origin = {
  path : Common.filename option; (* None for rules from the registry *)
  rules : Rule.rules;
  errors : Rule.invalid_rule_error list;
}

(* [rules_from_dashdash_config str] parse the config string
 * (using Semgrep_dashdash_config.config_kind_of_config_str)
 * and returns a list of rules_and_origin because the string can
 * correspond to a folder, in which case we return one rules_and_origin
 * per files in this folder.
 *)
val rules_from_dashdash_config : string -> rules_and_origin list
