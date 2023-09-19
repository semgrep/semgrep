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
open Testutil

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let get_and_check url =
  Logs.debug (fun m -> m "GET %s" url);
  let uri = Uri.of_string url in
  let response = Http_helpers.get uri in
  (* Check OK Status *)
  match response with
  | Ok body -> body
  | Error e -> failwith (Printf.sprintf "Error (%s): %s" url e)

let get_and_check_lwt url =
  Logs.debug (fun m -> m "(lwt) GET %s" url);
  let uri = Uri.of_string url in
  let%lwt response = Http_helpers.get_async uri in
  (* Check OK Status *)
  match response with
  | Ok body -> Lwt.return body
  | Error e -> failwith (Printf.sprintf "Error (%s): %s" url e)

let get_and_check_multi urls (f : string -> unit) =
  Logs.debug (fun m -> m "GET synchronously");
  urls |> Common.map get_and_check |> List.iter f

let get_and_check_multi_lwt ?(parallel = false) urls (f : string -> unit) =
  Logs.debug (fun m -> m "GET asynchronously (parallel)");
  let iter_fn = if parallel then Lwt_list.iter_p else Lwt_list.iter_s in
  Lwt_main.run
    (urls
    |> iter_fn (fun url ->
           let%lwt resp = get_and_check_lwt url in
           Lwt.return (f resp)))

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

(* For urls: google, github *)
(* Fails locally w/tls,tls-lwt installed *)
(* TLS failure: authentication failure: invalid certificate chain *)
let html_tests () =
  let urls =
    [ "https://www.google.com/"; "https://semgrep.dev/"; "https://github.com/" ]
  in
  let check_fn body =
    Alcotest.(check bool) "Body is not empty" true (String.length body <> 0)
  in
  let get_sync () = get_and_check_multi urls check_fn in

  let get_async () = get_and_check_multi_lwt urls check_fn in
  let get_async_parallel () =
    get_and_check_multi_lwt ~parallel:true urls check_fn
  in
  pack_tests "Basic HTML"
    [
      ("GET synchronously", get_sync);
      ("GET asynchronously", get_async);
      ("GET asynchronously (parallel)", get_async_parallel);
    ]

(* For urls: api.github.com  *)
(* Fails locally w/ tls,tls-lwt with the following error *)
(* TLS failure: authentication failure: invalid certificate chain *)
let json_tests () =
  let urls =
    [
      "https://api.github.com/";
      "https://dummyjson.com/products/1";
      "http://ip.jsontest.com";
    ]
  in
  let check_fn body =
    Alcotest.(check bool)
      "Body is not empty" true
      (Yojson.Safe.from_string body
      |> Yojson.Safe.Util.to_assoc |> List.length > 0)
  in
  let get_sync () = get_and_check_multi urls check_fn in
  let get_async () = get_and_check_multi_lwt urls check_fn in
  let get_async_parallel () =
    get_and_check_multi_lwt ~parallel:true urls check_fn
  in
  pack_tests "Basic JSON"
    [
      ("GET synchronously", get_sync);
      ("GET asynchronously", get_async);
      ("GET asynchronously (parallel)", get_async_parallel);
    ]

(* Confirmed all fails locally w/out tls,ssl,tls-lwt,ssl_lwt installed *)
(* Confirmed all works locally with ssl_lwt,ssl installed *)
let tests = pack_suites "OSemgrep Networking" [ html_tests (); json_tests () ]
