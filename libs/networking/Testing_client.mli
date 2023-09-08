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
            Testing_client.check_body body "./tests/foo/request.json"
            Lwt.return Testing_client.(basic_response "./tests/foo/response.json")
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

type test_response = { response : Cohttp_lwt.Response.t; body_path : string }
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
  ?status:int -> ?headers:Cohttp.Header.t -> string -> test_response
(** [basic_response ~status ~headers path_to_body] creates a [test_response]
  * with optional status and headers.
  * Example: [basic_response "./tests/FOO/BAR/response.json"].
  *)

val check_body : Cohttp_lwt.Body.t -> string -> unit Lwt.t
(** [check_body body path_to_body] will check a request/response has the
  * exact same bytes as the file [path_to_body]. Uses Alcotest to assert
  * Example: [check_body body "./tests/FOO/BAR/body.json"]
  *)

val check_method : Cohttp_lwt.Request.t -> string -> unit
(** [check_method request meth] will use Alcotest to assert a request
  * was made with a certain http method.
  * Example: [check_method request "GET"]
  *)

val check_header : Cohttp_lwt.Request.t -> string -> string -> unit
(** [check_header request header header_value] will use Alcotest to assert a request
  * was made with a certain header and value
  * Example: [check_header request "Authorization" "Bearer <token>"]
  *)

val get_header : Cohttp_lwt.Request.t -> string -> string option
(** [get_header request header] will return the value of a header in a request
  * or None if the header is not present
  * Example: [get_header request "Authorization"]
  *)

val with_testing_client : make_response_fn -> (unit -> unit) -> unit -> unit
(** [with_testing_client make_fn f ()]
  * will modify the [http_helpers] client to not actually make
  * http requests, but instead will replace it with a client
  * that uses [make_fn] to respond.
  * Any code inside [f] will be run with this modified client.
  *)
