val meth : string
(** method to match on: semgrep/search *)

val on_request :
  (Rule.rules -> Semgrep_output_v1_t.cli_match list) ->
  Jsonrpc.Structured.t option ->
  Yojson.Safe.t option
(** [on_request runner request] will run [runner] on the given pattern and optional
    language params. If the request is None, we return None. Otherwise, we return
    [Some (JSON response)] that contains the ranges of all matches *)
