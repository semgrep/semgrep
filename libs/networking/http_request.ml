(* Yosef Alsuhaibani
 *
 * Copyright (C) 2023 semgrep
 *)

open Common
open Cohttp
open Cohttp_lwt_unix

let logger = Logging.get_logger [ __MODULE__ ]

(* This type encodes a basic HTTP request; mainly used for in the secrets
 * post-processor; such that a basic http request like
 * GET semgrep.dev
 * Auth: ok
 * Type: tau
 * would be represented as
 * {
 *   url     = semgrep.dev/user;
 *   meth    = `GET;
 *   headers =
 *  [
 *    { n = Auth, v = ok};
 *    { n = Type, v = tau};
 *  ]
 * }
 * NOTE: we don't reuse cohttp's abstract type Cohttp.Headers.t; we still need
 * it to not be abstract for metavariable substitution.
 *)

type header = { name : string; value : string } [@@deriving show]
type meth = [ `DELETE | `GET | `POST | `HEAD | `PUT ] [@@deriving show]

(* why is url : string? metavariables (i.e http://$X) are present at parsing; which
 * if parsed with Uri.of_string translates it to http://%24x
 *)
type t = {
  url : string;
  meth : meth;
  headers : header list;
  body : string option;
}
[@@deriving show]

type response = { response_code : int; response_body : string }
[@@deriving show]

let to_cohttp_headers (headers : header list) =
  map (fun h -> (h.name, h.value)) headers |> Header.of_list

let perform_async (req : t) =
  let uri = Uri.of_string req.url in
  let meth = (req.meth :> Code.meth) in
  let headers = to_cohttp_headers req.headers in
  let body = Option.map (fun x -> (`String x :> Cohttp_lwt.Body.t)) req.body in
  let%lwt res, body = Client.call meth uri ~headers ?body in
  let%lwt response_body = Cohttp_lwt.Body.to_string body in
  Lwt.return
    {
      response_code = Response.status res |> Code.code_of_status;
      response_body;
    }

let perform_blocking (req : t) =
  (* cohttp-lwt-unix is super fragile, its behavior changes based on
     what packages you have installed. *)
  (* TLS exceptions that come from here can indicate that you are
     using ocaml-tls or ocaml-tls-lwt which doesn't support tls 1.3.
     To fix uninstall tls from opam. *)
  logger#linfo (lazy (spf "Http request: %s" (show req)));
  try
    let res = Lwt_main.run (perform_async req) in
    (* TODO: Investigate why these two don't show up in the logs. *)
    logger#info "Http response: %s" (show_response res);
    Result.ok res
  with
  | e ->
      logger#info "Http exception: %s" (Printexc.to_string e);
      Result.error (Exception.catch e)
