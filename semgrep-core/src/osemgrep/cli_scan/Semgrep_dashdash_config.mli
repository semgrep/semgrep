(* The --config argument can be:
 *  - a filename
 *  - TODO a registry entry,
 *  - TODO a URL,
 *  - TODO a folder name
 *  - TODO loads from defaults if None
 *)
val rules_from_dashdash_config :
  string -> Rule.rules * Rule.invalid_rule_error list
