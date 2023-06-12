val meth : string
(** method to match on: semgrep/search *)

val on_request :
  Runner_config.t ->
  Input_to_core_t.targets ->
  Jsonrpc.Structured.t option ->
  Yojson.Safe.t option
(** on a semgrep/search request, get the pattern and (optional) language params.
    We then try and parse the pattern in every language (or specified lang), and
    scan like normal, only returning the match ranges per file *)
