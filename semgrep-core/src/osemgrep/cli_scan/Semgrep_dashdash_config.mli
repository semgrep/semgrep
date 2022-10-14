type config_kind =
  | File of Common.filename
  | Dir of Common.dirname
  | R of registry_kind

and registry_kind =
  (* r/... *)
  | Registry of string
  (* p/... *)
  | Pack of string
  (* s/... *)
  | Snippet of string
  (* pad:basic *)
  | SavedSnippet of string (* username *) * string (* snippetname *)
[@@deriving show]

(* The --config string argument can resolve to one of the config_kind above *)
val rules_from_dashdash_config :
  string -> Rule.rules * Rule.invalid_rule_error list
