(* Commentary *)
(* This is a testing client to mock out http_helpers.ml so we
 * can check requests and insert responses. It works by replacing
 * the Cohttp client module with a custom one that calls
 * a make response function. The make response function
 * is called to check a request and create a response to
   return to the caller.
 * Example usage:
let with_foo_client =
    let make_fn = (fun req body ->
        match Uri.path (Cohttp.Request.uri req) with
        | "http://foo.com/api/v1/blah" ->
            Testing_client.check_method req "GET";
            Testing_client.check_body body "./tests/foo/request.json";
            let response_body =
              "./tests/ls/ci/response.json"
              |> Common.read_file
              |> Cohttp_lwt.Body.of_string in
            Lwt.return Testing_client.(basic_response response_body)
        | _ -> Alcotest.fail "unexpected request"
    )
    in
    Testing_client.with_testing_client make_fn
   ...

   let test_foo = ... in
   let tests =
   [
     ("Test foo", with_foo_client test_foo)
   ]
   in
   pack_tests "Foo Tests" tests
 *)

type test_response = {
  response : Cohttp_lwt.Response.t;
  body : Cohttp_lwt.Body.t;
}
(** [test_response] is a response (headers and status), and a path to a file
  * which will make the body of the response. The file is simply read and it's
  * exact bytes are returned
  *)

type make_response_fn =
  Cohttp_lwt.Request.t -> Cohttp_lwt.Body.t -> test_response Lwt.t
(** [make_response_fn requst body] takes in a request and its body, and
  * must return a response (see [basic_response]), and a path to a body
  * (see [test_response]).
  *)

val basic_response :
  ?status:Cohttp.Code.status_code ->
  ?headers:Cohttp.Header.t ->
  Cohttp_lwt.Body.t ->
  test_response
(** [basic_response ~status ~headers body] creates a [test_response]
  * with optional status and headers.
  * Example: [basic_response "{ \"api_code\": 304, \"info\": \"example\" }"].
  *)

val check_body : Cohttp_lwt.Body.t -> Cohttp_lwt.Body.t -> unit Lwt.t
(** [check_body expected_body actual_body] will check a request/response has the
  * exact same bytes as the file [path_to_body]. Uses Alcotest to assert the
  * bodies are equal.
  *)

val check_method : Cohttp.Code.meth -> Cohttp.Code.meth -> unit
(** [check_method expected_meth actual_meth] will use Alcotest to assert a request
  * was made with a certain http method.
  * Example: [check_method `GET request.meth]
  *)

val check_header : Cohttp_lwt.Request.t -> string -> string -> unit
(** [check_header request header header_value] will use Alcotest to assert a request
  * was made with a certain header and value
  * Example: [check_header request "Authorization" "Bearer <token>"]
  *)

val check_headers : Cohttp.Header.t -> Cohttp.Header.t -> unit
(** [check_header request header header_value] will use Alcotest to assert
  * two lists of headers are equal. Note that order and header capitlisation
  * does not matter---header lists which differ only in these aspects will
  * compare equal.
  *)

val with_testing_client : make_response_fn -> (unit -> unit) -> unit -> unit
(** [with_testing_client make_fn f ()]
  * will modify the [http_helpers] client to not actually make
  * http requests, but instead will replace it with a client
  * that uses [make_fn] to respond.
  * Any code inside [f] will be run with this modified client.
  *)

val client_from_file :
  string ->
  (module Cohttp_lwt.S.Client)
  * ((((Cohttp.Request.t * Cohttp_lwt.Body.t)
      * (Cohttp.Response.t * Cohttp_lwt.Body.t))
      list ->
     'a) ->
    'a)
(** [client_from_file "request-response-file.network"]
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
  * ignored. For example,
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
  *)
