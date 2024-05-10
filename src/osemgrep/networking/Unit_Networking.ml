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

let t = Testo.create

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let get_and_check_lwt caps url =
  Logs.debug (fun m -> m "GET %s" url);
  let uri = Uri.of_string url in
  (* Check OK Status *)
  match%lwt Http_helpers.get caps#network uri with
  | Ok { body = Ok body; _ } -> Lwt.return body
  | Ok { body = Error e; _ }
  | Error e ->
      Alcotest.failf "Error (%s): %s" url e

let post_and_check_lwt caps url body =
  Logs.debug (fun m -> m "POST %s" url);
  let uri = Uri.of_string url in
  (* Check OK Status *)
  match%lwt Http_helpers.post ~body caps#network uri with
  | Ok { body = Ok body; _ } -> Lwt.return body
  | Ok { body = Error e; _ }
  | Error e ->
      Alcotest.failf "Error (%s): %s" url e

let get_and_check_multi_lwt ?(parallel = false) caps urls (f : string -> unit) =
  Logs.debug (fun m ->
      m "GET (%s)" (if parallel then "parallel" else "sequential"));
  let iter_fn = if parallel then Lwt_list.iter_p else Lwt_list.iter_s in
  Lwt_platform.run
    (urls
    |> iter_fn (fun url ->
           let%lwt resp = get_and_check_lwt caps url in
           Lwt.return (f resp)))

let post_and_check_multi_lwt ?(parallel = false) caps
    (url_body_pairs : (string * string) list) (f : string -> unit) =
  Logs.debug (fun m ->
      m "POST (%s)" (if parallel then "parallel" else "sequential"));
  let iter_fn = if parallel then Lwt_list.iter_p else Lwt_list.iter_s in
  Lwt_platform.run
    (url_body_pairs
    |> iter_fn (fun (url, body) ->
           let%lwt resp = post_and_check_lwt caps url body in
           Lwt.return (f resp)))

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let html_tests caps =
  let urls =
    [ "https://www.google.com/"; "https://semgrep.dev/"; "https://github.com/" ]
  in
  let check_fn body =
    Alcotest.(check bool) "Body is not empty" true (String.length body <> 0)
  in
  let get () = get_and_check_multi_lwt caps urls check_fn in
  let get_parallel () =
    get_and_check_multi_lwt ~parallel:true caps urls check_fn
  in
  Testo.categorize "Basic HTML" [ t "GET" get; t "GET (parallel)" get_parallel ]

let json_tests caps =
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
  let get () = get_and_check_multi_lwt caps urls check_fn in
  let get_parallel () =
    get_and_check_multi_lwt ~parallel:true caps urls check_fn
  in
  Testo.categorize "Basic JSON" [ t "GET" get; t "GET (parallel)" get_parallel ]

let post_tests caps =
  let url_body_pairs = [ ("https://postman-echo.com/post", "") ] in
  let check_fn body =
    Alcotest.(check bool) "Body is not empty" true (String.length body <> 0)
  in
  let post () = post_and_check_multi_lwt caps url_body_pairs check_fn in
  let post_parallel () =
    post_and_check_multi_lwt ~parallel:true caps url_body_pairs check_fn
  in
  Testo.categorize "Basic POST"
    [ t "POST" post; t "POST (parallel)" post_parallel ]

(* TODO: We should use the mock network here, so we can re-enable these tests *)
let tests caps =
  Testo.categorize_suites "OSemgrep Networking"
    [ html_tests caps; json_tests caps; post_tests caps ]
