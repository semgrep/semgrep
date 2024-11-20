(*
  This is a testing client to mock out http_helpers.ml so we
  can check requests and insert responses. It works by replacing
  the Cohttp client module with a custom one that calls
  a make_response function. The make_response function
  is called to check a request and create a response to
  return to the caller.

  Example usage:
  let with_foo_client =
    let make_response_fn = (fun req body ->
        match Uri.path (Cohttp.Request.uri req) with
        | "http://foo.com/api/v1/blah" ->
            Http_mock_client.check_method `GET req.meth;
            Http_mock_client.check_body body "./tests/foo/request.json";
            let response_body =
              "./tests/ls/ci/response.json"
              |> Common.read_file
              |> Cohttp_lwt.Body.of_string in
            Lwt.return Http_mock_client.(basic_response response_body)
        | _ -> Alcotest.fail "unexpected request"
    )
    in
    Http_mock_client.with_testing_client make_response_fn
   ...

   let test_foo = ... in
   let tests =
   [
     ("Test foo", with_foo_client test_foo)
   ]
   in
   pack_tests "Foo Tests" tests
 *)

type test_response = { response : Cohttp.Response.t; body : Cohttp_lwt.Body.t }
(** [test_response] is a response (headers and status), and body *)

type make_response_fn =
  Cohttp.Request.t -> Cohttp_lwt.Body.t -> test_response Lwt.t
(** [make_response_fn requst body] takes in a request and its body, and
  * must return a response (see [basic_response]), and a body
  * (see [test_response]).
  *)

val basic_response :
  ?status:int -> ?headers:Cohttp.Header.t -> Cohttp_lwt.Body.t -> test_response
(** [basic_response ~status ~headers body] creates a [test_response]
  * with optional status and headers.
  * Example: [basic_response "{ \"api_code\": 304, \"info\": \"example\" }"].
  *)

val body_of_file : ?trim:bool -> Fpath.t -> Cohttp_lwt.Body.t
(** [body_of_file trim path_to_body] will return a body which is the exact bytes
  * of the file [path_to_body], unless [trim] is true, in which case the bytes
  * will be trimmed of leading and trailing whitespace.
  *)

val check_body : Cohttp_lwt.Body.t -> Cohttp_lwt.Body.t -> unit Lwt.t
(** [check_body expected_body actual_body] will check a request/response has the
  * exact same bytes. Uses Alcotest to assert the bodies are equal.
  *)

val check_method : Cohttp.Code.meth -> Cohttp.Code.meth -> unit
(** [check_method expected_meth actual_meth] will use Alcotest to assert a
  * request was made with a certain http method.
  * Example: [check_method `GET request.meth]
  *)

val check_header : Cohttp.Request.t -> string -> string -> unit
(** [check_header request header header_value] will use Alcotest to assert a
  * request was made with a certain header and value
  * Example: [check_header request "Authorization" "Bearer <token>"]
  *)

val check_headers : Cohttp.Header.t -> Cohttp.Header.t -> unit
(** [check_header request header header_value] will use Alcotest to assert
  * two lists of headers are equal. Note that order and header capitlisation
  * does not matter---header lists which differ only in these aspects will
  * compare equal.
  *)

val get_header : Cohttp.Request.t -> string -> string option
(** [get_header request header] will return the value of a header in a request
  * or None if the header is not present
  * Example: [get_header request "Authorization"]
  *)

val with_testing_client : make_response_fn -> (unit -> 'a) -> unit -> 'a
(** [with_testing_client make_response_fn f ()]
  * will modify the [http_helpers] client to not actually make
  * http requests, but instead will replace it with a client
  * that uses [make_response_fn] to respond.
  * Any code inside [f] will be run with this modified client.
  *)

val client_from_file :
  Fpath.t ->
  (module Cohttp_lwt.S.Client)
  * ((((Cohttp.Request.t * Cohttp_lwt.Body.t)
      * (Cohttp.Response.t * Cohttp_lwt.Body.t))
      list ->
     'a) ->
    'a)
(** [client_from_file (Fpath.v "request-response-file.network")]
  * will return a new http client which may be used in lieu of the
  * [http_helpers] client, which rather than performing network requests will
  * instead look for a matching request in the given file and yield the
  * given paired response. If no request matches, a test failure will be
  * raised with Alcotest.
  *
  * The file format is
  * ```
  * > REQUEST (possibly multiple lines)
  * < RESPONSE (possibly multiple lines)
  * ```
  * repeated as needed, with lines beginning with charaters other than < or >
  * ignored.
  * A request/response above is essentially just the bytes/text comprising the
  * HTTP frame, with the exception of using normal line endings intead of \r\n.
  * For example,
  * ```
  * > POST /api/v4 HTTP/1.1
  * > Host: www.github.com
  * > User-Agent: curl/8.1.2
  * > Accept: */*
  * >
  * > secret:very legit secret
  * < HTTP/1.1 200 OK
  * < Date: Thu, 07 Sep 2023 21:29:57 GMT
  * <
  * ```
  * NOTE: currently, blank lines should still have "> " or "< ", i.e., there
  * should be a space after the indicator.
  *
  * The easiest way to create a file in this format is to make a request using
  * curl, with the -v flag. For example, `curl -v google.com` would give
  * *   Trying 142.251.46.206:80...
  * * Connected to google.com (142.251.46.206) port 80 (#0)
  * > GET / HTTP/1.1
  * > Host: google.com
  * > User-Agent: curl/8.1.2
  * > Accept: */*
  * >
  * < HTTP/1.1 301 Moved Permanently
  * < [snipped, more headers]
  * <
  * <HTML><HEAD><meta content="text/html;charset=utf-8">
  * <TITLE>301 Moved</TITLE></HEAD><BODY>
  * <H1>301 Moved</H1>
  * The document has moved
  * <A HREF="http://www.google.com/">here</A>.
  * </BODY></HTML>
  * * Connection #0 to host google.com left intact
  * This can be used almost directly: the only change required is to prefix the
  * response body (bzw. request body, if it were non-empty) with "< "
  * (bzw. "> ").
  *)
