(* ex: "p/python" *)
type config_str = string [@@deriving show]

(* config_str in a parsed form *)
type config_kind =
  (* ex: 'foo.yaml' *)
  | File of Common.filename
  (* ex: 'myrules/' (will go also recursively in subdirs of myrules) *)
  | Dir of Common.filename
  (* ex: 'https://raw.githubusercontent.com/r2c/semgrep-rules/template.yaml' *)
  | URL of Uri.t
  | R of registry_kind

and registry_kind =
  (* r/... *)
  | Registry of string
  (* p/... *)
  | Pack of string
  (* s/... *)
  | Snippet of string
  (* ex: 'pad:basic' *)
  | SavedSnippet of string (* username *) * string (* snippetname *)
  (* shortcuts *)
  (* "p/default" *)
  | Auto
  (* p/r2c *)
  | R2c
  (* Semgrep App shortcuts *)
  | Policy
  | SupplyChain
[@@deriving show]

val config_kind_of_config_str : config_str -> config_kind
val url_of_registry_kind : registry_kind -> Uri.t
