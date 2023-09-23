type rules_and_origin = {
  rules : Rule.rules;
  errors : Rule.invalid_rule_error list;
  origin : origin;
}

and origin =
  (* For rules that come from a local file, this is the path to the file
   * containing the rule. It's used to create a dotted rule identifier
   * such as 'stuff.rules.foo' for a rule declared as 'foo' in the file
   * 'stuff/rules.yml' (see the ~rewrite_rule_ids parameters below).
   *
   * This path may seem redundant with the third argument of
   * load_rules_from_file() below, but sometimes this argument corresponds to
   * a temporary file generated from content fetched from the network
   * (e.g., rules coming from the registry locally stored in /tmp/) in which
   * case we do not want to add this prefix, hence the use of Other_origin in
   * that case, even if we're still parsing a local (temporary) file.
   *)
  | Local_file of Fpath.t
  (* usually for rules coming from the registry where no rewrite_rule_ids
   * is needed (they already come with an adjusted rule_id)
   *)
  | Other_origin
[@@deriving show]

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
  rewrite_rule_ids:bool ->
  token_opt:Auth.token option ->
  registry_caching:bool ->
  Rules_config.t ->
  rules_and_origin list Lwt.t

(* [rules_from_dashdash_config] returns a list of rules_and_origin
 * because the [Rules_config.t] can be a [Dir], in which case we return one
 * rules_and_origin per files in this folder.
 *)
val rules_from_dashdash_config :
  rewrite_rule_ids:bool ->
  token_opt:Auth.token option ->
  registry_caching:bool ->
  Rules_config.t ->
  rules_and_origin list

(* low-level API *)
val load_rules_from_file :
  origin:origin -> registry_caching:bool -> Fpath.t -> rules_and_origin

val load_rules_from_url :
  ?token_opt:Auth.token option -> ?ext:string -> Uri.t -> rules_and_origin
