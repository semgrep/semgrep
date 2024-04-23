val start_meth : string
(** method to match on: semgrep/search *)

val mk_params :
  lang:Xlang.t option ->
  fix:string option ->
  includes:string list ->
  excludes:string list ->
  string ->
  Jsonrpc.Structured.t

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
