(* Austin Theriault
 *
 * Copyright (C) 2019-2023 Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

module Net = Cohttp_lwt_unix.Net
module Request = Cohttp_lwt.Request
module Response = Cohttp_lwt.Response
module Body = Cohttp_lwt.Body
module Header = Cohttp.Header

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type test_response = { response : Response.t; body_path : string }
type make_response_fn = Request.t -> Body.t -> test_response Lwt.t

module type S = sig
  val make_response : make_response_fn
end

module Make (M : S) : Cohttp_lwt.S.Client = struct
  open M
  include Cohttp_lwt_unix.Client

  let call ?(ctx = Net.default_ctx) ?headers ?(body = `Empty) ?chunked meth uri
      =
    ignore ctx;
    let headers =
      match headers with
      | None -> Header.init ()
      | Some h -> h
    in
    let chunked =
      match chunked with
      | Some c -> c
      | None -> false
    in
    let req = Request.make_for_client ~headers ~chunked meth uri in
    Logs.debug (fun m ->
        m "[Testing client] Request: %s"
          (Request.sexp_of_t req |> Sexplib.Sexp.to_string_hum));
    let%lwt _body = Body.to_string body in
    Logs.debug (fun m -> m "[Testing client] Body: %s" _body);
    let%lwt response = make_response req body in
    let response_body =
      response.body_path |> Common.read_file |> Cohttp_lwt.Body.of_string
    in
    Lwt.return (response.response, response_body)

  let get ?ctx ?headers uri = call ?ctx ?headers `GET uri

  let delete ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `DELETE uri

  let post ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `POST uri

  let put ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `PUT uri

  let patch ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `PATCH uri
end

(*****************************************************************************)
(* Helper Functions *)
(*****************************************************************************)

let basic_response ?(status = 200) ?(headers = Header.init ()) body_path =
  let status = Cohttp.Code.status_of_code status in
  let response = Response.make ~status ~headers ~flush:true () in
  { response; body_path }

let check_body body path =
  let expected_body = path |> Common.read_file in
  let%lwt actual_body = Cohttp_lwt.Body.to_string body in
  (* Read file above adds a newline :( *)
  Alcotest.(check string) "body" expected_body (actual_body ^ "\n");
  Lwt.return_unit

let check_method req meth =
  Alcotest.(check string)
    "method" meth
    (Cohttp.Request.meth req |> Cohttp.Code.string_of_method)

let check_header req header header_val =
  let actual_header = Cohttp.Header.get (Cohttp.Request.headers req) header in
  match actual_header with
  | None ->
      Alcotest.fail
        (Printf.sprintf "header %s not found. Headers: %s" header
           (Cohttp.Header.to_string (Cohttp.Request.headers req)))
  | Some actual_header ->
      Alcotest.(check string) "header" header_val actual_header

let get_header req header =
  Cohttp.Header.get (Cohttp.Request.headers req) header

(*****************************************************************************)
(* Entrypoint *)
(*****************************************************************************)

let with_testing_client make_fn test_fn () =
  let prev_client = !Http_helpers.client_ref in
  let new_client : (module Cohttp_lwt.S.Client) =
    (module Make (struct
      let make_response = make_fn
    end))
  in
  Http_helpers.client_ref := new_client;
  test_fn ();
  Http_helpers.client_ref := prev_client
