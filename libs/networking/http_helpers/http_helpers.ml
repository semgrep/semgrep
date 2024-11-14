open Cohttp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A few helpers to perform http GET and POST requests.
 *
 * Below we separate the methods out by async (returns Lwt promise),
 * and sync (runs async method in lwt runtime)
 * This way we can use the async methods in the language server,
 * and other places too.
 *
 * Note that using [@@profiling] with xxx_async function is useless
 * as the actual computation is done in the caller doing the
 * Lwt_main.run
 *)

let src = Logs.Src.create "networking.http"

module Log = (val Logs.src_log src : Logs.LOG)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type body_result = (string, string) result

type server_response = {
  body : body_result;
  response : Cohttp.Response.t;
  code : int;
}

type client_result = (server_response, string) result

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(* Create a client reference so we can swap it out with a testing version *)

let client_ref : (module Cohttp_lwt.S.Client) option ref = ref None
let in_mock_context = ref false
let set_client_ref v = if not !in_mock_context then client_ref := Some v

let with_client_ref v f x =
  let old = !client_ref in
  set_client_ref v;
  let result = f x in
  (match old with
  | Some old -> set_client_ref old
  | None -> client_ref := None);
  result

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let string_of_meth = Cohttp.Code.string_of_method

let server_response_of_response (response, body) meth =
  let code = response |> Response.status |> Code.code_of_status in
  let meth_str = string_of_meth meth in
  match code with
  | _ when Code.is_success code -> { body = Ok body; response; code }
  | _ when Code.is_error code ->
      Log.debug (fun m -> m "HTTP %s failed:\n %s" meth_str body);
      { body = Error body; response; code }
  (* This case is anything that is [Code.is_redirection] or [Code.is_informational]*)
  | _ ->
      Log.debug (fun m -> m "HTTP %s unexpected response:\n %s" meth_str body);
      { body = Error body; response; code }

(*****************************************************************************)
(* Proxy Stuff *)
(*****************************************************************************)

(* Respect the proxy!*)
(* Cohttp doesn't https://github.com/mirage/ocaml-cohttp/issues/459 *)
(* https://github.com/0install/0install/blob/6c0f5c51bc099370a367102e48723a42cd352b3b/ocaml/zeroinstall/http.cohttp.ml#L232-L256 *)

(* What do we have to do to support a proxy? *)
(* On Native OCaml (not js), the certificate MUST be in a path listed in
   https://github.com/mirage/ca-certs/blob/fa4ff942f1ac980e2502a0783ef10ade5ba497f2/lib/ca_certs.ml#L34-L50 *)
(* Or set SSL_CERT_FILE=path/to/cert. But note setting this means no default certs will be included *)
(* HTTP(s)_PROXY MUST have a scheme, http:// or https://, and must have a port
   if it isn't on port 80/443 *)
(* So HTTP_PROXY=http://localhost:8080 or
   HTTPS_PROXY=https://localhost:8080 *)
(* And note that HTTP_PROXY is only used for HTTP requests! We almost
   exclusively use HTTPS urls so make sure both are set*)
let get_proxy uri =
  let proxy_uri_env =
    match Uri.scheme uri with
    (* TODO support all versions of these env vars. They can be both uppercase *)
    (* and lowercase *)
    | Some "http" -> Some "HTTP_PROXY"
    | Some "https" -> Some "HTTPS_PROXY"
    | _ -> None
  in
  let proxy_uri = Option.bind proxy_uri_env Sys.getenv_opt in
  match proxy_uri with
  | Some proxy_uri ->
      Log.info (fun m -> m "Using proxy %s for request" proxy_uri);
      Some (Uri.of_string proxy_uri)
  | None -> None

(* Why this wrapper function? Client.call takes a uri, and some other things
   and then makes a Request.t with said uri and sends that request to the same
   uri By using Client.callv, we can make a request that has some uri, but then
   really send it to a different uri. This is used for proxying requests *)
let default_resp_handler (response, body) =
  let%lwt body_str = Cohttp_lwt.Body.to_string body in
  Lwt.return (response, body_str)

(* Why do we need a response_handler? From the cohttp docs: *)
(*
    [response_body] is not buffered, but stays on the wire until
        consumed. It must therefore be consumed in a timely manner.
        Otherwise the connection would stay open and a file descriptor leak
        may be caused. Following responses would get blocked.
        Functions in the {!Body} module can be used to consume [response_body]. *)
(* So if we don't handle the body, we can leak file descriptors and accidentally keep the connection open *)
(* Let's just handle the body when making the request then, so we don't risk leaving this up*)
(* to a consumer of this library, who may or may not know about this requirement *)
let call_client ?(body = Cohttp_lwt.Body.empty) ?(headers = [])
    ?(chunked = false) ?(resp_handler = default_resp_handler) meth url =
  let module Client =
    (val match !client_ref with
         | Some client -> client
         | None -> failwith "HTTP client not initialized")
  in
  (* Send the request to the proxy, not the original url, if it's set *)
  let%lwt content_length_header =
    match meth with
    | `POST ->
        let%lwt length, _ = Cohttp_lwt.Body.length body in
        (* Not added when using callv :(, so we gotta add it here *)
        Lwt.return [ ("content-length", Int64.to_string length) ]
    | _ -> Lwt.return []
  in
  let headers = content_length_header @ headers in
  let headers = Header.of_list headers in
  (* [make_for_client] Sets host header internally *)
  let req = Cohttp.Request.make_for_client ~headers ~chunked meth url in
  let req, url =
    match get_proxy url with
    | Some proxy_url ->
        ({ req with Cohttp.Request.resource = Uri.to_string url }, proxy_url)
    | None -> (req, url)
  in
  let stream_req = Lwt_stream.of_list [ (req, body) ] in
  (* callv is deprecated in 6.0.0 of cohttp, but that's not released yet as of this comment *)
  let responses_stream_opt =
    (* We add a try catch  to handle otherwise uncaught unix errors
       (e.g. ECONNREFUSED) and return a more helpful error message.

       Currently, we're observing high failure rates from our metrics endpoint
       with the corresponding error `Unix_error: Connection reset by peer read`
       when the server initiates a connection with SSL via TLS v1.2 instead
       of v1.3

       For investigative work, we're currently using an internal AWS Lambda
       URL instead of our human-friendly metrics endpoint. This is because the
       Lambda URL tends to get matched to a node with TLS v1.3 support,
       whereas our metrics endpoint for some reason almost exclusively is
       matched with a node that initiaties TLS v1.2 which is causing the
       connection reset error.

       This is a somewhat temporary workaround as even our Lambda URL will not
       always respond with TLS v1.3, as we are not guaranteed to hit the same
       node. Currently, AWS does not support specifying a minimum TLS version
       of v1.3, and we will need to figure out a better solution for ensuring
       reliable metrics delivery. *)
    try%lwt
      let%lwt responses_stream = Client.callv url stream_req in
      Lwt.return_ok responses_stream
    with
    | exn ->
        let err = Printexc.to_string exn in
        Log.err (fun m ->
            m "HTTP %s to '%s' failed: %s" (string_of_meth meth)
              (Uri.to_string url) err);
        Lwt.return_error err
  in
  let handle_responses responses_stream =
    (* Assume that we only get one response back *)
    match%lwt
      responses_stream |> Lwt_stream.map_s resp_handler |> Lwt_stream.to_list
    with
    | [ (response, response_body) ] -> Lwt.return_ok (response, response_body)
    | [] -> Lwt.return_error "No responses from a single request"
    | _ :: _ -> Lwt.return_error "Multiple responses from a single request"
    | exception Cohttp_lwt.Connection.Retry ->
        Lwt.return_error
          "Error in request: maybe the server hung up prematurely?"
  in
  Lwt_result.bind responses_stream_opt handle_responses

(*****************************************************************************)
(* Async *)
(*****************************************************************************)
let rec get ?(headers = []) caps url =
  Log.info (fun m -> m "GET on %s" (Uri.to_string url));
  (* This checks to make sure a client has been set *)
  (* Instead of defaulting to a client, as that can cause *)
  (* Hard to debug build and runtime issues *)
  let response_result = call_client ~headers `GET url in
  let handle_response (response, body) =
    let server_response = server_response_of_response (response, body) `GET in
    match server_response.code with
    (* Automatically resolve redirects, in this case a 307 Temporary Redirect.
       This is important for installing the Semgrep Pro Engine binary, which
       receives a temporary redirect at the proper endpoint.
    *)
    | 301
    | 302
    | 307
    | 308 -> (
        let location = Header.get (response |> Response.headers) "location" in
        match location with
        | None ->
            let code_str = Code.string_of_status response.status in
            let err = "HTTP GET failed: " ^ code_str ^ ":\n" ^ body in
            Log.err (fun m -> m "%s" err);
            let server_response = { server_response with body = Error err } in
            Lwt.return_ok server_response
        | Some url -> get caps (Uri.of_string url))
    | _ -> Lwt.return_ok server_response
  in
  Lwt_result.bind response_result handle_response
[@@profiling]

let post ~body ?(headers = [ ("content-type", "application/json") ])
    ?(chunked = false) _caps url =
  Log.info (fun m -> m "POST on %s" (Uri.to_string url));
  let response =
    call_client
      ~body:(Cohttp_lwt.Body.of_string body)
      ~headers ~chunked `POST url
  in
  Lwt_result.bind response (fun (response, body) ->
      Lwt.return_ok (server_response_of_response (response, body) `POST))
[@@profiling]
