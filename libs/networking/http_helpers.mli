val get_async :
  ?headers:(string * string) list -> Uri.t -> (string, string) result Lwt.t
(** [get_async ~headers uri] retrieves [uri] (via HTTP GET) with the provided
    [headers], asynchronously. The return value is either a promise of [Ok body] - if the request was
    successful, or an error message. *)

val post_async :
  body:string ->
  ?headers:(string * string) list ->
  Uri.t ->
  (string, int * string) result Lwt.t
(** [post_async ~body ~headers uri] sends a POST request to [uri] with the provided
    [headers] (default: content-type: application/json) and [body] asynchronously. The returned
    value is a promise of either [Ok body] if the request was successful, or an
    [Error (code, msg)], including the HTTP status [code] and a message. *)

val get : ?headers:(string * string) list -> Uri.t -> (string, string) result
(** [get ~headers uri] retrieves [uri] (via HTTP GET) with the provided
    [headers]. The return value is either [Ok body] - if the request was
    successful, or an error message. *)

val post :
  body:string ->
  ?headers:(string * string) list ->
  Uri.t ->
  (string, int * string) result
(** [post ~body ~headers uri] sends a POST request to [uri] with the provided
    [headers] (default: content-type: application/json) and [body]. The returned
    value is either [Ok body] if the request was successful, or an
    [Error (code, msg)], including the HTTP status [code] and a message. *)
