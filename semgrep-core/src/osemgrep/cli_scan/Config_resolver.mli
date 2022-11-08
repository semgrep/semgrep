type rules_and_origin = {
  path : Common.filename option; (* None for remote files *)
  rules : Rule.rules;
  errors : Rule.invalid_rule_error list;
}

val rules_from_dashdash_config : string -> rules_and_origin list
