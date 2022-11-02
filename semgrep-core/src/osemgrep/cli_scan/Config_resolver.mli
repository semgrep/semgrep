(* The --config string argument can resolve to one of the config_kind *)
val rules_from_dashdash_config :
  string -> Rule.rules * Rule.invalid_rule_error list
