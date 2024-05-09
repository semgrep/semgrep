(* This module provides a simple interface for making HTTP requests. It wraps
   Cohttp with better error handling, adds network mocking, and proxy support *)

type body_result = (string, string) result
(** [response_body] is [Ok body] when the server returns an a success status
    code. A success status is any 2xx status code [Code.is_success]. Otherwise
    it is an [Error body]. The body will not be modified either way.
  *)

type server_response = {
  body : body_result;
  response : Cohttp.Response.t;
  code : int;
}
(** [server_response] is whatever the server returns *)

type client_result = (server_response, string) result
(** [client_result] is [Ok response] when the network request is successful.
    This does not guarantee the server response was an Ok status code. It
    just means we made a network response and got /some/ response  *)

(* Before we didn't wrap anything here in two results. It used to be one result
 * with a string error message. This was a mistake. We were trusting cohttp to
 * not blow up on the smallest stuff, instead of returning a proper error. So
 * now we have a proper error type. That Cohttp should have done for us. *)

val call_client :
  ?body:Cohttp_lwt.Body.t ->
  ?headers:(string * string) list ->
  ?chunked:bool ->
  ?resp_handler:
    (Cohttp.Response.t * Cohttp_lwt.Body.t ->
    (Cohttp.Response.t * string) Lwt.t) ->
  Cohttp.Code.meth ->
  Uri.t ->
  (Cohttp.Response.t * string, string) result Lwt.t
(** [call_client] is a low-level function that sends an HTTP request to the
    provided URI. It returns a promise of either [Ok (response, body)] if the
    request was successful, or an [Error msg] if the request failed.
    [resp_handler] can be used to directly work with the response body, as it
    has to be handled directly on the stream. By default it will convert the
    body to a string *)

val get :
  ?headers:(string * string) list ->
  Cap.Network.t ->
  Uri.t ->
  client_result Lwt.t
(** [get_async ~headers caps uri] retrieves [uri] (via HTTP GET) with the
    provided [headers], asynchronously. The return value is either a promise
    of [Ok body] - if the request was successful, or an error message.
    If a temporary redirect (307) is returned, this function will automatically
    re-query and resolve the redirection.
   *)

val post :
  body:string ->
  ?headers:(string * string) list ->
  ?chunked:bool ->
  Cap.Network.t ->
  Uri.t ->
  client_result Lwt.t
(** [post_async ~body ~headers ~chunked caps uri] asynchronously sends a
    POST request to [uri] with
    - [headers] (default: content-type: application/json)
    - [chunked] (default: false) this maps to whether we enable
      "Transfer-Encoding: chunked" in our outgoing request, which can be useful
       for streaming a large body of text, but not all servers support it.
       We err on the side of caution and disable it by default.
       Reference: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Transfer-Encoding#directives
    - [body] payload to send (e.g. JSON body as string)

    The returned value is a promise of either [Ok body] if the request was
    successful, or an [Error (code, msg)], including the HTTP status [code]
    and a message. *)

val set_client_ref : (module Cohttp_lwt.S.Client) -> unit
(** [set_client_ref] sets a reference to the Cohttp client module used by the
    functions in this module. By default, it is set to
    [Cohttp_lwt_unix.Client], but can be changed to an instance
    of [TestingClient] if you want to test things. *)

val with_client_ref : (module Cohttp_lwt.S.Client) -> ('a -> 'b) -> 'a -> 'b
(** [with_client client f x] is a helper function that temporarily sets the client
    reference to the provided client module, runs the provided function, and
    then resets the client reference to its original value. *)

(* See Http_mock_client.ml. If this global is set, set_client_ref()
 * above will be a noop (and so leave the mock_http_client in place).
 *)
val in_mock_context : bool ref
