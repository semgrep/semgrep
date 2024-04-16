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

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type get_info = { response : Cohttp.Response.t; code : int }

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(* Create a client reference so we can swap it out with a testing version *)

let client_ref : (module Cohttp_lwt.S.Client) option ref = ref None
let in_mock_context = ref false
let set_client_ref v = if not !in_mock_context then client_ref := Some v

(*****************************************************************************)
(* Async *)
(*****************************************************************************)

(* We use a functor here so that we can pass in what we use for the Lwt runtime
 * This is platform dependent (JS vs Unix), so we can't just choose one.
 *
 * alt: we could use a ref like above, but this doesn't need to be decided at
 * runtime, only at build, so I don't want to open the door to being able to
 * change it at runtime for no reason.
 * alt: we also can't just call Lwt_platform directly, as when
 * compiling/linking a package OCaml requires a choice of a virtual module
 * implementation, and it can't defer to the thing that's using the package.
 *)

module Make (Lwt_platform : sig
  val run : 'a Lwt.t -> 'a
end) =
struct
  (* Respect the proxy!*)
  (* Cohttp doesn't https://github.com/mirage/ocaml-cohttp/issues/459 *)
  (* https://github.com/0install/0install/blob/6c0f5c51bc099370a367102e48723a42cd352b3b/ocaml/zeroinstall/http.cohttp.ml#L232-L256 *)
  let get_proxy uri =
    let proxy_uri_env =
      match Uri.scheme uri with
      | Some "http" -> Some "HTTP_PROXY"
      | Some "https" -> Some "HTTPS_PROXY"
      | _ -> None
    in
    let proxy_uri = Option.bind proxy_uri_env Sys.getenv_opt in
    match proxy_uri with
    | Some proxy_uri ->
        Logs.debug (fun m -> m "Using proxy %s for request" proxy_uri);
        Some (Uri.of_string proxy_uri)
    | None -> None

  (* Why this wrapper function? Client.call takes a uri, and some other things *)
  (* and then makes a Request.t with said uri and sends that request to the same uri *)
  (* By using Client.callv, we can make a request that has some uri, but then really *)
  (* send it to a different uri. This is used for proxying requests *)

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
    let req = Cohttp.Request.make_for_client ~headers ~chunked meth url in
    let req, url =
      match get_proxy url with
      | Some proxy_url ->
          ({ req with Cohttp.Request.resource = Uri.to_string url }, proxy_url)
      | None -> (req, url)
    in
    let stream_req = Lwt_stream.of_list [ (req, body) ] in
    (* callv is deprecated in 6.0.0 of cohttp, but that's not released yet as of this comment *)
    let%lwt responses_stream = Client.callv url stream_req in
    (* Assume that we only get one response back *)
    (* MUST USE GET HERE *)
    let%lwt repsonses =
      responses_stream |> Lwt_stream.map_s resp_handler |> Lwt_stream.to_list
    in
    match repsonses with
    | [ (response, response_body) ] -> Lwt.return (response, response_body)
    | [] -> failwith "No responses from a single request"
    | _ -> failwith "Multiple responses from a single request"

  let rec get_async ?(headers = []) caps url =
    Logs.debug (fun m -> m "GET on %s" (Uri.to_string url));
    (* This checks to make sure a client has been set *)
    (* Instead of defaulting to a client, as that can cause *)
    (* Hard to debug build and runtime issues *)
    let%lwt response, body = call_client ~headers `GET url in
    let code = response |> Response.status |> Code.code_of_status in
    match code with
    | _ when Code.is_success code -> Lwt.return (Ok (body, { code; response }))
    (* Automatically resolve redirects, in this case a 307 Temporary Redirect.
       This is important for installing the Semgrep Pro Engine binary, which
       receives a temporary redirect at the proper endpoint.
    *)
    | 307 -> (
        let location = Header.get (response |> Response.headers) "location" in
        match location with
        | None ->
            let code_str = Code.string_of_status response.status in
            let err = "HTTP GET failed: " ^ code_str ^ ":\n" ^ body in
            Logs.debug (fun m -> m "%s" err);
            Lwt.return (Error (err, { code; response }))
        | Some url -> get_async caps (Uri.of_string url))
    | _ when Code.is_error code ->
        let code_str = Code.string_of_status response.status in
        let err = "HTTP GET failed: " ^ code_str ^ ":\n" ^ body in
        Logs.debug (fun m -> m "%s" err);
        Lwt.return (Error (err, { code; response }))
    | _ ->
        let code_str = Code.string_of_status response.status in
        let err = "HTTP GET unexpected response: " ^ code_str ^ ":\n" ^ body in
        Logs.debug (fun m -> m "%s" err);
        Lwt.return (Error (err, { code; response }))

  let post_async ~body ?(headers = [ ("content-type", "application/json") ])
      ?(chunked = false) _caps url =
    Logs.debug (fun m -> m "POST on %s" (Uri.to_string url));
    let%lwt response, body =
      call_client
        ~body:(Cohttp_lwt.Body.of_string body)
        ~headers ~chunked `POST url
    in
    let code = response |> Response.status |> Code.code_of_status in
    match code with
    | _ when Code.is_success code -> Lwt.return (Ok body)
    | _ when Code.is_error code ->
        let code_str = Code.string_of_status response.status in
        let err = "HTTP POST failed: " ^ code_str ^ ":\n" ^ body in
        Logs.debug (fun m -> m "%s" err);
        Lwt.return (Error (code, err))
    | _ ->
        let code_str = Code.string_of_status response.status in
        let err = "HTTP POST unexpected response: " ^ code_str ^ ":\n" ^ body in
        Logs.debug (fun m -> m "%s" err);
        Lwt.return (Error (code, err))

  (*****************************************************************************)
  (* Sync *)
  (*****************************************************************************)

  (* TODO: extend to allow to curl with JSON as answer *)
  let get ?headers caps url = Lwt_platform.run (get_async ?headers caps url)
  [@@profiling]

  let post ~body ?(headers = [ ("content-type", "application/json") ])
      ?(chunked = false) caps url =
    (* We add an exception handler to handle otherwise uncaught unix errors
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
       reliable metrics delivery.
    *)
    Lwt_platform.run
      (Lwt.catch
         (fun () -> post_async ~body ~headers ~chunked caps url)
         (fun exn ->
           let err = Printexc.to_string exn in
           (* NOTE: the caller will have the responsibility to handle and log the error
              with the appropriate log level serverity (e.g. warn / error / app)
           *)
           Logs.debug (fun m ->
               m "send to '%s' failed: %s" (Uri.to_string url) err);
           Lwt.return (Error (0, err))))
  [@@profiling]
end
