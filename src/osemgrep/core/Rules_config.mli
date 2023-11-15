(* ex: "p/python" *)
type config_string = string [@@deriving show]

(* config_string in a parsed form *)
type t =
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

(* the in_docker parameter is useful just for better error reporting *)
val parse_config_string : in_docker:bool -> config_string -> t
