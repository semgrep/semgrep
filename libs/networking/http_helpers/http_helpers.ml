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

(* SAFETY: This value is currently only used in single-threaded contexts, and
   relies on this for safety. If you need to use this in a multi-threaded
   context you should evaluate switching to Eio-based networking or some other
   mechanism for a fibre-local or otherwise non-global networking client. Note
   that DLS is likely not appropriate due to Eio work-stealing and a global
   Mutex is likely not appropriate due to uses of with_client_ref. *)
let client_ref : (module Cohttp_lwt.S.Client) option ref = ref None
let set_client_ref v = client_ref := Some v

let with_client_ref v f x =
  let old = !client_ref in
  Common.protect ~finally:(fun () -> client_ref := old) @@ fun () ->
  set_client_ref v;
  f x

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

(* Why this wrapper function? Client.call takes a uri, and some other things
   and then makes a Request.t with said uri and sends that request to the same
   uri By using Client.callv, we can make a request that has some uri, but then
   really send it to a different uri. This is used for proxying requests *)
let default_resp_handler (response, body) =
  let%lwt body_str = Cohttp_lwt.Body.to_string body in
  Lwt.return (response, body_str)

(* Why do we need a response_handler? From the cohttp docs:

    [response_body] is not buffered, but stays on the wire until
        consumed. It must therefore be consumed in a timely manner.
        Otherwise the connection would stay open and a file descriptor leak
        may be caused. Following responses would get blocked.
        Functions in the {!Body} module can be used to consume [response_body].

   So if we don't handle the body, we can leak file descriptors and
   accidentally keep the connection open. Let's just handle the body when making
   the request then, so we don't risk leaving this up to a consumer of this
   library, who may or may not know about this requirement. *)
let call_client ?(body = Cohttp_lwt.Body.empty) ?(headers = [])
    ?(chunked = false) ?(resp_handler = default_resp_handler) meth url =
  let module Client : Cohttp_lwt.S.Client =
    (val match !client_ref with
         | Some client -> client
         | None -> failwith "HTTP client not initialized")
  in
  let%lwt content_length_header =
    match meth with
    | `POST ->
        let%lwt length, _ = Cohttp_lwt.Body.length body in
        (* Not added when using callv :(, so we gotta add it here *)
        Lwt.return [ ("content-length", Int64.to_string length) ]
    | _ -> Lwt.return []
  in
  let headers = Header.of_list (content_length_header @ headers) in
  match%lwt
    (* We add a catch additional exceptions beyond just Cohttp's Retry
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
    Client.call ~headers ~body ~chunked meth url
  with
  | response, response_body ->
      let%lwt resp = resp_handler (response, response_body) in
      Lwt.return_ok resp
  | exception Cohttp_lwt.Connection.Retry ->
      Lwt.return_error "Error in request: maybe the server hung up prematurely?"
  | exception exn ->
      let err = Printexc.to_string exn in
      Log.err (fun m ->
          m "HTTP %s to '%s' failed: %s" (string_of_meth meth)
            (Uri.to_string url) err);
      Lwt.return_error err

(*****************************************************************************)
(* Async *)
(*****************************************************************************)
let rec get ?(headers = []) caps url =
  Log.info (fun m -> m "GET on %s" (Uri.to_string url));
  (* This checks to make sure a client has been set instead of defaulting to a
     client, as that can cause hard to debug build and runtime issues *)
  let response_result = call_client ~headers `GET url in
  let handle_response (response, body) =
    let server_response = server_response_of_response (response, body) `GET in
    match server_response.code with
    (* Automatically resolve redirects, in this case a 307 Temporary Redirect.
       This is important for installing the Semgrep Pro Engine binary, which
       receives a temporary redirect at the proper endpoint. *)
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

let put ~body ?(headers = [ ("content-type", "application/json") ])
    ?(chunked = false) _caps url =
  Log.info (fun m -> m "PUT on %s" (Uri.to_string url));
  let response =
    call_client
      ~body:(Cohttp_lwt.Body.of_string body)
      ~headers ~chunked `PUT url
  in
  Lwt_result.bind response (fun (response, body) ->
      Lwt.return_ok (server_response_of_response (response, body) `PUT))
[@@profiling]
