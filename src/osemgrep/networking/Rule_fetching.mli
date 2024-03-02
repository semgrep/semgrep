type rules_and_origin = {
  rules : Rule.rules;
  errors : Rule.invalid_rule_error list;
  origin : origin;
}

and origin =
  (* For rules created at the command line, e.g., with -e *)
  | CLI_argument
  (* For rules that come from a local file, this is the path to the file
   * containing the rule. Beyond just tracking rule origin, it is used to
   * create a dotted rule identifier such as 'stuff.rules.foo' for a rule
   * declared as 'foo' in the file 'stuff/rules.yml' (see the ~rewrite_rule_ids
   * parameters below).
   *
   * This path may seem redundant with the third argument of
   * load_rules_from_file() below, but sometimes this argument corresponds to a
   * temporary file generated from content fetched from the network (e.g.,
   * rules coming from the registry locally stored in /tmp/) in which case we
   * still want to track the "true" origin of the rule, for instance, to know
   * that it came from the registry, even if we're still parsing a local
   * (temporary) file. Ideally we would not persist such files to disk before
   * parsing them (mostly to avoid TOC/TOU issues), but this is not currently a
   * easy change.
   *)
  | Local_file of Fpath.t
  (* For rules which originate from the registry (both pro and OSS)
   * alt: of Uri.t or registry_config_kind?
   *)
  | Registry
  (* For rules which are fetched from the app. These are specific to a cloud
   * platform configuration, so additional information need not be tracked here
   * TODO: maybe it does? since policy/supply_chain/code/secrets?
   *)
  | App
  (* For rules which come from other remote URIs. Rules from other sources may
   * not be able to use all features (e.g., secrets validators) without
   * additional flags to opt-in, due to security considerations.
   *)
  | Untrusted_remote of Uri.t
[@@deriving show]

val partition_rules_and_errors :
  rules_and_origin list -> Rule.rules * Rule.invalid_rule_error list

val rules_from_pattern :
  string * Xlang.t option * string option -> rules_and_origin list

(* [rules_from_rules_source] returns rules from --config or -e.
 * If [rewrite_rule_ids] is true, it will add the path of the config
 * file to the start of rule_ids.
 * The token is used to fetch App rules (e.g., from --config policy).
 *)
val rules_from_rules_source :
  token_opt:Auth.token option ->
  rewrite_rule_ids:bool ->
  strict:bool ->
  < Cap.network > ->
  Rules_source.t ->
  rules_and_origin list

(* TODO: make cap network an option (with token) *)
val rules_from_rules_source_async :
  token_opt:Auth.token option ->
  rewrite_rule_ids:bool ->
  strict:bool ->
  < Cap.network > ->
  Rules_source.t ->
  rules_and_origin list Lwt.t

(* internals *)

val rules_from_dashdash_config_async :
  rewrite_rule_ids:bool ->
  token_opt:Auth.token option ->
  < Cap.network ; .. > ->
  Rules_config.t ->
  (rules_and_origin list * Rule.error list) Lwt.t

(* [rules_from_dashdash_config] returns a list of rules_and_origin
 * because the [Rules_config.t] can be a [Dir], in which case we return one
 * rules_and_origin per files in this folder.
 *)
val rules_from_dashdash_config :
  rewrite_rule_ids:bool ->
  token_opt:Auth.token option ->
  < Cap.network ; .. > ->
  Rules_config.t ->
  rules_and_origin list * Rule.error list

(* low-level API *)
val load_rules_from_file :
  rewrite_rule_ids:bool ->
  origin:origin ->
  < Cap.network ; .. > ->
  Fpath.t ->
  (rules_and_origin, Rule.error) Result.t

val load_rules_from_url :
  origin:origin ->
  ?token_opt:Auth.token option ->
  ?ext:string ->
  < Cap.network ; .. > ->
  Uri.t ->
  (rules_and_origin, Rule.error) Result.t
