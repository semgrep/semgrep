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

(* Commentary *)
(*  *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

module Net = Cohttp_lwt_unix.Net
module Request = Cohttp_lwt.Request
module Response = Cohttp_lwt.Response
module Body = Cohttp_lwt.Body
module Header = Cohttp.Header

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

(* List of expected requests in order, replies in order*)

type test_response = { response : Response.t; body_path : string }
type check_request_fn = Request.t -> Body.t -> unit
type make_response_fn = Request.t -> Body.t -> test_response

module type S = sig
  val check_request : Request.t -> Body.t -> unit
  val make_response : Request.t -> Body.t -> test_response
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
    check_request req body;
    let response = make_response req body in
    let response_body =
      response.body_path |> Common.read_file |> Cohttp_lwt.Body.of_string
    in
    Lwt.return (response.response, response_body)
end

let basic_response ?(status = `OK) ?(headers = Header.init ()) body_path =
  let response = Response.make ~status ~headers ~flush:true () in
  { response; body_path }

let with_testing_client test_fn check_fn make_fn =
  let prev_client = !Http_helpers.client_ref in
  let new_client : (module Cohttp_lwt.S.Client) =
    (module Make (struct
      let check_request = check_fn
      let make_response = make_fn
    end))
  in
  Http_helpers.client_ref := new_client;
  test_fn ();
  Http_helpers.client_ref := prev_client
