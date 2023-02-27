(* input *)
type rules_source =
  (* -e/-l/--replacement. In theory we could even parse the string to get
   * a XPattern.t *)
  | Pattern of string * Xlang.t * string option (* replacement *)
  (* --config. In theory we could even parse the string to get
   * some Semgrep_dashdash_config.config_kind list *)
  | Configs of Semgrep_dashdash_config.config_str list
(* TODO? | ProjectUrl of Uri.t? or just use Configs for it? *)
[@@deriving show]

(* output *)
type rules_and_origin = {
  origin : origin;
  rules : Rule.lazy_rule list;
  errors : Rule.invalid_rule_error list;
}

and origin = Common.filename option (* None for remote files *)
[@@deriving show]

val partition_rules_and_errors :
  rules_and_origin list -> Rule.lazy_rule list * Rule.invalid_rule_error list

(* [rules_from_rules_source] returns rules from --config or -e
 * TODO: does it rewrite the rule_id?
 *)
val rules_from_rules_source : rules_source -> rules_and_origin list

(* internals *)

(* [rules_from_dashdash_config config] returns a list of rules_and_origin
 * because the string can correspond to a folder, in which case we return
 * one rules_and_origin per files in this folder.
 *)
val rules_from_dashdash_config :
  Semgrep_dashdash_config.config_kind -> rules_and_origin list

(* low-level API *)
val load_rules_from_file : Common.filename -> rules_and_origin
val load_rules_from_url : Uri.t -> rules_and_origin
