type config_kind =
  (* ex: 'foo.yaml' *)
  | File of Common.filename
  (* ex: 'myrules/' (will go also recursively in subdirs of myrules) *)
  | Dir of Common.filename
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
[@@deriving show]

val config_kind_of_config_str : string -> config_kind
val url_of_registry_kind : registry_kind -> Uri.t
