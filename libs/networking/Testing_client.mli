type test_response = { response : Cohttp_lwt.Response.t; body_path : string }
type check_request_fn = Cohttp_lwt.Request.t -> Cohttp_lwt.Body.t -> unit

type make_response_fn =
  Cohttp_lwt.Request.t -> Cohttp_lwt.Body.t -> test_response

val basic_response :
  ?status:Cohttp.Code.status_code ->
  ?headers:Cohttp.Header.t ->
  string ->
  test_response

val with_testing_client :
  (unit -> unit) -> check_request_fn -> make_response_fn -> unit
