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

val config_kind_of_config_str : string -> config_kind
val url_of_registry_kind : registry_kind -> Uri.t
