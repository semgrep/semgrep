val start_meth : string
(** method to match on: semgrep/search *)

val ongoing_meth : string
(** method to match on: semgrep/searchOngoing *)

val start_search :
  RPC_server.t ->
  Jsonrpc.Structured.t option ->
  Yojson.Safe.t option * RPC_server.t
(** [start_search server params] will cause a search to start with the given parameters,
    storing the information of remaining rules/targets to search in the server session.
    It will then return the matches in the first file with matches.
    Will return `Assoc ["locations": `List []] when the search has concluded.
  *)

val search_next_file :
  RPC_server.t ->
  Jsonrpc.Structured.t option ->
  Yojson.Safe.t option * RPC_server.t
(** [search_next_file server params] is used during an ongoing search, and will
    return the matches in the first file with matches, based on the remaining
    rules/targets to search in the server session state.
    Will return `Assoc ["locations": `List []] when the search has concluded.
  *)
