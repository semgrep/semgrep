open Http_lwt_client

(* The http-lwt-client package offers:
   - HTTP/1 and HTTP/2 support (using http/af and h2)
   - TLS support (using OCaml-TLS)
   - IPv4 and IPv6 connection establishment (via happy-eyeballs)
*)

(* The HTTP lwt client library uses happy_eyeballs as the underlying
   layer for establishing connections. It uses DNS, and comes with a small DNS
   cache. We use a single happy_eyeballs instance to reuse the cache present.
*)
let happy_eyeballs = Happy_eyeballs_lwt.create ()

(* TODO: extend to allow to curl with JSON as answer *)
let get ?headers url =
  let bodyf _ acc data = Lwt.return (acc ^ data) in
  let promise = request ~happy_eyeballs ?headers (Uri.to_string url) bodyf "" in
  let r = Lwt_main.run promise in
  match r with
  | Ok (response, content) when Status.is_successful response.status ->
      Ok content
  | Ok (response, _) ->
      Error
        ("HTTP request failed, server response "
        ^ Status.to_string response.status)
  | Error (`Msg msg) ->
      let err = "HTTP request failed: " ^ msg in
      Logs.debug (fun m -> m "%s" err);
      Error err
  [@@profiling]

let post ~body ?(headers = [ ("content-type", "application/json") ]) url =
  let bodyf _ acc data = Lwt.return (acc ^ data) in
  let promise =
    request ~happy_eyeballs ~meth:`POST ~headers ~body (Uri.to_string url) bodyf
      ""
  in
  let r = Lwt_main.run promise in
  match r with
  | Ok (response, content) when Status.is_successful response.status ->
      Ok content
  | Ok (response, _) ->
      Error
        ( Status.to_code response.status,
          "HTTP request failed, server response "
          ^ Status.to_string response.status )
  | Error (`Msg msg) ->
      let err = "HTTP request failed: " ^ msg in
      Logs.debug (fun m -> m "%s" err);
      Error (-1, err)
  [@@profiling]
