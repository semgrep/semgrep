val start_meth : string
(** method to match on: semgrep/search *)

val mk_params :
  lang:Analyzer.t option ->
  fix:string option ->
  includes:string list ->
  excludes:string list ->
  string ->
  Jsonrpc.Structured.t

val ongoing_meth : string
(** method to match on: semgrep/searchOngoing *)

val start_search :
  Session.t ->
  Jsonrpc.Id.t ->
  Jsonrpc.Structured.t option ->
  Session.t * Lsp_.Reply.t
(** [start_search server params] will cause a search to start with the given parameters,
    storing the information of remaining rules/targets to search in the server session.
    It will then return the matches in the first file with matches.
    Will return `Assoc ["locations": `List []] when the search has concluded.
  *)

val search_next_file :
  Session.t ->
  Jsonrpc.Id.t ->
  Jsonrpc.Structured.t option ->
  Session.t * Lsp_.Reply.t
(** [search_next_file server params] is used during an ongoing search, and will
    return the matches in the first file with matches, based on the remaining
    rules/targets to search in the server session state.
    Will return `Assoc ["locations": `List []] when the search has concluded.
  *)
