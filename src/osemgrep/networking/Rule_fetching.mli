type rules_and_origin = {
  origin : origin;
  rules : Rule.rules;
  errors : Rule.invalid_rule_error list;
}

and origin = Fpath.t option (* None for remote config *) [@@deriving show]

val partition_rules_and_errors :
  rules_and_origin list -> Rule.rules * Rule.invalid_rule_error list

(* [rules_from_rules_source] returns rules from --config or -e.
 * If [rewrite_rule_ids] is true, it will add the path of the config
 * file to the start of rule_ids.
 * The token is used to fetch App rules (e.g., from --config policy).
 *)
val rules_from_rules_source :
  token_opt:Auth.token option ->
  rewrite_rule_ids:bool ->
  registry_caching:bool ->
  Rules_source.t ->
  rules_and_origin list

(* internals *)

val rules_from_dashdash_config_async :
  token_opt:Auth.token option ->
  registry_caching:bool ->
  Semgrep_dashdash_config.config_kind ->
  rules_and_origin list Lwt.t

(* [rules_from_dashdash_config] returns a list of rules_and_origin
 * because the [config_kind] can be a [Dir], in which case we return one
 * rules_and_origin per files in this folder.
 *)
val rules_from_dashdash_config :
  token_opt:Auth.token option ->
  registry_caching:bool ->
  Semgrep_dashdash_config.config_kind ->
  rules_and_origin list

(* low-level API *)
val load_rules_from_file : registry_caching:bool -> Fpath.t -> rules_and_origin

val load_rules_from_url :
  ?token_opt:Auth.token option -> ?ext:string -> Uri.t -> rules_and_origin
