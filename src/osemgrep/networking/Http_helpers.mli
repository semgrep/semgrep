(** [get ~headers uri] retrieves [uri] (via HTTP GET) with the provided
    [headers]. The return value is either [Ok body] - if the request was
    successful, or an error message. *)
val get : ?headers:(string * string) list -> Uri.t -> (string, string) result

(** [post ~body ~headers uri] sends a POST request to [uri] with the provided
    [headers] (default: content-type: application/json) and [body]. The returned
    value is either [Ok body] if the request was successful, or an
    [Error (code, msg)], including the HTTP status [code] and a message. *)
val post :
  body:string ->
  ?headers:(string * string) list ->
  Uri.t ->
  (string, int * string) result
