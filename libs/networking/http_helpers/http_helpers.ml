(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
open Cohttp
open Common
open Lwt.Infix

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

(* Create a client reference so we can swap it out with a testing version.  In
 * order to not make relevant Testo tests serial, this is a domain-local value,
 * though in practice it's a global singleton value. *)
let client_ref : (module Cohttp_lwt.S.Client) option Domain.DLS.key =
  (* Set `split_from_parent` to the identity here, so that we properly
     retain HTTP client references even when spawning child domains.
   *)
  Domain.DLS.new_key ~split_from_parent:Fun.id (Fun.const None)

let set_client_ref r = Domain.DLS.set client_ref (Some r)

let with_client_ref v f x =
  let old = Domain.DLS.get client_ref in
  Common.protect ~finally:(fun () -> Domain.DLS.set client_ref old) @@ fun () ->
  set_client_ref v;
  f x

(* Create a swappable call function for Eio-based networking so we can
   swap it out with a testing version for mocking.
 *)
type eio_call_fn =
  sw:Eio.Switch.t ->
  headers:Header.t ->
  body:Cohttp_eio.Body.t ->
  chunked:bool ->
  Http.Method.t ->
  Uri.t ->
  Response.t * Cohttp_eio.Body.t

(* This is a Hook we use so we can swap out our HTTP responses with a mocked one,
   for testing purposes.
   Currently, this is only used in LSP, but we plan to port our Lwt-based HTTP
   functionality to Eio eventually, at which point we can deprecate the above
   `client_ref` mechanism.
 *)
let hook_eio_call_fn : eio_call_fn option Hook.t = Hook.create None

let with_hook_eio_call_fn_set v f =
  Hook.with_hook_set hook_eio_call_fn (Some v) f

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

let default_resp_handler_eio (response, body) =
  let body_str = Eio.Flow.read_all body in
  Ok (response, body_str)

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
(* coupling(eio-port): if you change this you must change the eio version *)
let call_client ?(body = Cohttp_lwt.Body.empty) ?(headers = [])
    ?(chunked = false) ?(resp_handler = default_resp_handler) ?timeout_secs meth
    url =
  let module Client : Cohttp_lwt.S.Client =
    (val match Domain.DLS.get client_ref with
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

  let resp =
    Lwt.catch
      (fun () ->
        Client.call ~headers ~body ~chunked meth url
        >>= resp_handler >>= Lwt.return_ok)
      (function
        | Cohttp_lwt.Connection.Retry ->
            Lwt.return_error
              (spf "HTTP %s failed: connection reset or timed out"
                 (string_of_meth meth))
        | exn ->
            let err = Printexc.to_string exn in
            (* Include the exception constructor (e.g. [Unix_error],
               [Failure], [Tls_alert]) so the Err log and the caller-
               visible error both tell you what kind of failure it was.
               [exn_slot_name] yields a stable identifier without any of
               the free-form payload that [Printexc.to_string] may embed
               (which can include the URL via cohttp/TLS messages). *)
            let stable_msg =
              spf "HTTP %s failed: %s" (string_of_meth meth)
                (Printexc.exn_slot_name exn)
            in
            Logs_.msg_with_detail ~src Logs.Error stable_msg (fun () ->
                spf "url=%s exn=%s" (Uri.to_string url) err);
            Lwt.return_error stable_msg)
  in
  let timeout_secs = Option.value timeout_secs ~default:10.0 in
  let timeout =
    Lwt_unix.sleep timeout_secs >>= fun () ->
    (* URL is Debug-only; the returned error carries only method + seconds. *)
    Log.debug (fun m ->
        m "HTTP %s to '%s' timed out after %.1f seconds" (string_of_meth meth)
          (Uri.to_string url) timeout_secs);
    Lwt.return_error
      (spf "HTTP %s timed out after %.1f seconds" (string_of_meth meth)
         timeout_secs)
  in
  Lwt.pick [ resp; timeout ]

(* coupling(eio-port): if you change this you must change the lwt version *)
let call_eio_client ?(body = Cohttp.Body.empty) ?(headers = [])
    ?(chunked = false) ?(resp_handler = default_resp_handler_eio) meth url =
  Eio.Switch.run (fun sw ->
      let call_fn =
        match Hook.get hook_eio_call_fn with
        | Some call_fn -> call_fn
        | None -> failwith "EIO HTTP call_fn not initialized"
      in
      (* Send the request to the proxy, not the original url, if it's set *)
      let content_length_header =
        match meth with
        | `POST ->
            let length = Cohttp.Body.length body in
            (* Not added when using callv :(, so we gotta add it here *)
            [ ("content-length", Int64.to_string length) ]
        | _ -> []
      in
      let headers = Header.of_list (content_length_header @ headers) in
      let body = Cohttp_eio.Body.of_string (Body.to_string body) in
      try
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
        call_fn ~sw ~headers ~body ~chunked meth url |> resp_handler
      with
      (* Note: `Cohttp_eio.Client.call` raises plain [Failure] for several
         conditions, and the message typically embeds the request URI (e.g.,
         "no host specified (in <uri>)", "HTTPS not enabled (for <uri>)",
         "Proxy could not form tunnel to <uri>"). We therefore fall [Failure]
         through to the general handler below, which keeps the detail at
         Debug and returns a URL-free error to the caller. *)
      | exn ->
          let err = Printexc.to_string exn in
          (* See the Lwt variant above. *)
          let stable_msg =
            spf "HTTP %s failed: %s" (string_of_meth meth)
              (Printexc.exn_slot_name exn)
          in
          Logs_.msg_with_detail ~src Logs.Error stable_msg (fun () ->
              spf "url=%s exn=%s" (Uri.to_string url) err);
          Error stable_msg)

(*****************************************************************************)
(* Async *)
(*****************************************************************************)
(* coupling(eio-port): if you change this you must change the eio version *)
let rec get ?(headers = []) ?timeout_secs url =
  Logs_.msg_with_detail ~src Logs.Info
    (spf "GET %s" (Uri.host_with_default ~default:"<no-host>" url))
    (fun () -> Uri.to_string url);
  (* This checks to make sure a client has been set instead of defaulting to a
     client, as that can cause hard to debug build and runtime issues *)
  let response_result = call_client ~headers ?timeout_secs `GET url in
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
            (* Response body is server-controlled content that may include
               tokens, identifiers, or URLs the server put in its error
               payload. Keep it at Debug only; log and return a URL- and
               body-free error. *)
            let err = spf "HTTP GET failed: %s" code_str in
            Logs_.msg_with_detail ~src Logs.Error err (fun () ->
                spf "%s body=%s" err body);
            let server_response = { server_response with body = Error err } in
            Lwt.return_ok server_response
        | Some url -> get ?timeout_secs (Uri.of_string url))
    | _ -> Lwt.return_ok server_response
  in
  Lwt_result.bind response_result handle_response
[@@profiling]

(* coupling(eio-port): if you change this you must change the lwt version *)
let rec get_eio ?(headers = []) url =
  Logs_.msg_with_detail ~src Logs.Info
    (spf "GET %s" (Uri.host_with_default ~default:"<no-host>" url))
    (fun () -> Uri.to_string url);
  (* This checks to make sure a client has been set *)
  (* Instead of defaulting to a client, as that can cause *)
  (* Hard to debug build and runtime issues *)
  let response_result = call_eio_client ~headers `GET url in
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
            (* See the Lwt variant above. *)
            let err = spf "HTTP GET failed: %s" code_str in
            Logs_.msg_with_detail ~src Logs.Error err (fun () ->
                spf "%s body=%s" err body);
            let server_response = { server_response with body = Error err } in
            Ok server_response
        | Some url -> get_eio (Uri.of_string url))
    | _ -> Ok server_response
  in
  response_result |> Result.map handle_response |> Result.join
[@@profiling]

(* coupling(eio-port): if you change this you must change the eio version *)
let post ~body ?(headers = [ ("content-type", "application/json") ])
    ?(chunked = false) url =
  Logs_.msg_with_detail ~src Logs.Info
    (spf "POST %s" (Uri.host_with_default ~default:"<no-host>" url))
    (fun () -> Uri.to_string url);
  let response =
    call_client
      ~body:(Cohttp_lwt.Body.of_string body)
      ~headers ~chunked `POST url
  in
  Lwt_result.bind response (fun (response, body) ->
      Lwt.return_ok (server_response_of_response (response, body) `POST))
[@@profiling]

(* coupling(eio-port): if you change this you must change the lwt version *)
let post_eio ~body ?(headers = [ ("content-type", "application/json") ])
    ?(chunked = false) url =
  Logs_.msg_with_detail ~src Logs.Info
    (spf "POST %s" (Uri.host_with_default ~default:"<no-host>" url))
    (fun () -> Uri.to_string url);
  let response =
    call_eio_client
      ~body:(Cohttp.Body.of_string body)
      ~headers ~chunked `POST url
  in
  response
  |> Result.map (fun (response, body) ->
      server_response_of_response (response, body) `POST)
[@@profiling]

let put ~body ?(headers = [ ("content-type", "application/json") ])
    ?(chunked = false) url =
  Logs_.msg_with_detail ~src Logs.Info
    (spf "PUT %s" (Uri.host_with_default ~default:"<no-host>" url))
    (fun () -> Uri.to_string url);
  let response =
    call_client
      ~body:(Cohttp_lwt.Body.of_string body)
      ~headers ~chunked `PUT url
  in
  Lwt_result.bind response (fun (response, body) ->
      Lwt.return_ok (server_response_of_response (response, body) `PUT))
[@@profiling]

(*****************************************************************************)
(* Eio *)
(*****************************************************************************)

(* In order to do networking with Eio, we need to create a client.
   This is done on a per-env environment (one per domain), so we isolate it
   into a helper to be invoked elsewhere.
   This is only invoked by `mk_eio_call_fn` below, which will persist the
   client in its closure.
 *)
let mk_eio_client env =
  let https =
    let authenticator =
      match Ca_certs.authenticator () with
      | Ok x -> x
      | Error msg -> (
          match msg with
          | `Msg x ->
              failwith (Format.asprintf "Failed to load CA certificates: %s" x))
    in
    let tls_config =
      match Tls.Config.client ~authenticator () with
      | Ok tls_config -> tls_config
      | Error (`Msg e) ->
          failwith (Common.spf "Error creating TLS config: %s" e)
    in
    fun uri raw ->
      let host =
        Uri.host uri
        |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
      in
      Tls_eio.client_of_flow ?host tls_config raw
  in
  Cohttp_eio.Client.make (* THINK? *) ~https:(Some https) (Eio.Stdenv.net env)

let mk_eio_call_fn client =
 fun ~sw ~headers ~body ~chunked meth uri ->
  Cohttp_eio.Client.call ~sw ~headers ~body ~chunked client meth uri
