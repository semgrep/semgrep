(* ex: "p/python" *)
type config_string = string [@@deriving show]

(* config_string in a parsed form *)
type config_kind =
  (* ex: 'foo.yaml' *)
  | File of Fpath.t
  (* ex: 'myrules/' (will go also recursively in subdirs of myrules) *)
  | Dir of Fpath.t
  (* ex: 'https://raw.githubusercontent.com/r2c/semgrep-rules/template.yaml' *)
  | URL of Uri.t
  | R of registry_config_kind
  | A of app_config_kind

and registry_config_kind =
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
and app_config_kind =
  (* this requires also SEMGREP_REPO_NAME to be set *)
  | Policy
  | SupplyChain
[@@deriving show]

val parse_config_string : config_string -> config_kind
