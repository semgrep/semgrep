val client_ref : (module Cohttp_lwt.S.Client) option ref
(** [client_ref] is a reference to the Cohttp client module used by the
    functions in this module. By default, it is set to
    [Cohttp_lwt_unix.Client], but can be changed to an instance
    of [TestingClient] if you want to test things. *)

val set_client_ref : (module Cohttp_lwt.S.Client) -> unit

(* See Http_mock_client.ml. If this global is set, set_client_ref()
 * above will be a noop (and so leave the mock_http_client in place).
 *)
val in_mock_context : bool ref

type get_info = {
  response : Cohttp.Response.t;
  body : Cohttp_lwt.Body.t;
  code : int;
}

module Make (I : sig
  val run : 'a Lwt.t -> 'a
end) : sig
  val get_async :
    ?headers:(string * string) list ->
    Uri.t ->
    (string * get_info, string * get_info) result Lwt.t
  (** [get_async ~headers uri] retrieves [uri] (via HTTP GET) with the provided
    [headers], asynchronously. The return value is either a promise of [Ok body] - if the request was
    successful, or an error message.
    If a temporary redirect (307) is returned, this function will automatically
    re-query and resolve the redirection.
   *)

  val post_async :
    body:string ->
    ?headers:(string * string) list ->
    ?chunked:bool ->
    Uri.t ->
    (string, int * string) result Lwt.t
  (** [post_async ~body ~headers ~chunked uri] asynchronously sends a POST request to [uri] with
    [headers] (default: content-type: application/json)
    [chunked] (default: false) this maps to whether we enable "Transfer-Encoding: chunked"
      in our outgoing request, which can be useful for streaming a large body of text, but not
      all servers support it. We err on the side of caution and disable it by default.
      Reference: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Transfer-Encoding#directives
    [body] payload to send (e.g. JSON body as string)
    The returned value is a promise of either [Ok body] if the request was successful,
    or an [Error (code, msg)], including the HTTP status [code] and a message. *)

  val get :
    ?headers:(string * string) list ->
    Uri.t ->
    (string * get_info, string * get_info) result
  (** [get ~headers uri] retrieves [uri] (via HTTP GET) with the provided
    [headers]. The return value is either [Ok body] - if the request was
    successful, or an error message.
    If a temporary redirect (307) is returned, this function will automatically
    re-query and resolve the redirection.
   *)

  val post :
    body:string ->
    ?headers:(string * string) list ->
    ?chunked:bool ->
    Uri.t ->
    (string, int * string) result
  (** [post ~body ~headers ~chunked uri] sends a POST request to [uri] with
    [headers] (default: content-type: application/json)
    [chunked] (default: false)
    [body] payload to send (e.g. JSON body as string)
    The returned value is either [Ok body] if the request was successful, or an
    [Error (code, msg)], including the HTTP status [code] and a message. *)
end
