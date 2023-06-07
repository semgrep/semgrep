open Http_lwt_client

(* happy eyeballs is an Internet standard
   (https://datatracker.ietf.org/doc/html/rfc8305) and an OCaml package which
   purpose is to establish a TCP connection, independent of the internet
   protocol version:
   - the input is a hostname (and a port), the output is either a file
     descriptor or an error
   - it does DNS resolution (for both A (IPv4) and AAAA (IPv6)) - 3 attempts
     with a timeout of "resolve_timeout" (here: 2 seconds)
   - it then attempts to establish to the IPv6 address(es) and IPv4 address(es)
     (with a preference to IPv6), using a 10 seconds timeout ("connect_timeout",
     defaults to 10 seconds)

   The HTTP lwt client library uses happy_eyeballs as the underlying
   layer for establishing connections.
*)
let happy_eyeballs =
  let happy_eyeballs =
    Happy_eyeballs.create ~resolve_timeout:(Duration.of_sec 2)
      (Mtime_clock.elapsed_ns ())
  in
  Happy_eyeballs_lwt.create ~happy_eyeballs ()

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
  | Error (`Msg msg) -> Error ("HTTP request failed: " ^ msg)
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
  | Error (`Msg msg) -> Error (-1, "HTTP request failed: " ^ msg)
  [@@profiling]
