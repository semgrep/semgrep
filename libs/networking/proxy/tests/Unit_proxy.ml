(*
   Copyright (c) Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Tests for [Proxy.settings_from_env].
 *
 * The interesting cases are the malformed ones: cohttp raises
 * [Invalid_argument "No host was provided in URI ..."] on a proxy URI without
 * a host, which used to escape all the way to the OCaml runtime and kill the
 * scan with a stack trace (ENGINE-2208). An empty value is now ignored
 * as curl does, and anything else we can't turn into a usable URI is
 * reported as an error instead of being handed to cohttp.
 *)

let t = Testo.create

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* [Proxy.getenv] prefers the lowercase variant of a variable and either
 * variant may already be set in the environment the test suite runs in, so we
 * pin both to the value under test. *)
let with_proxy_env ?(http_proxy = "") ?(https_proxy = "") ?(all_proxy = "") f =
  let bindings =
    [
      ("HTTP_PROXY", http_proxy);
      ("HTTPS_PROXY", https_proxy);
      ("ALL_PROXY", all_proxy);
    ]
    |> List.concat_map (fun (var, value) ->
        [
          (String.lowercase_ascii var, value);
          (String.uppercase_ascii var, value);
        ])
  in
  List.fold_left
    (fun f (var, value) () -> Testutil_mock.with_setenv var value f)
    f bindings ()

let settings_from_env ?http_proxy ?https_proxy ?all_proxy () =
  with_proxy_env ?http_proxy ?https_proxy ?all_proxy Proxy.settings_from_env

let get_ok (result : (Proxy.settings, [ `Msg of string ]) result) :
    Proxy.settings =
  match result with
  | Ok settings -> settings
  | Error (`Msg msg) ->
      Alcotest.fail ("expected valid proxy settings but got: " ^ msg)

let get_error (result : (Proxy.settings, [ `Msg of string ]) result) : string =
  match result with
  | Ok _ -> Alcotest.fail "expected invalid proxy settings but got Ok"
  | Error (`Msg msg) -> msg

let check_uri msg expected (actual : Uri.t option) =
  Alcotest.(check (option string))
    msg expected
    (Option.map Uri.to_string actual)

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

(* The bug we are fixing: an empty variable is common for "no proxy" in CI
   systems, and it used to produce a hostless URI that crashed cohttp. *)
let test_empty_values_are_ignored () =
  let { Proxy.http_proxy; https_proxy; all_proxy; _ } =
    get_ok (settings_from_env ())
  in
  check_uri "HTTP_PROXY" None http_proxy;
  check_uri "HTTPS_PROXY" None https_proxy;
  check_uri "ALL_PROXY" None all_proxy

let test_whitespace_only_values_are_ignored () =
  let { Proxy.https_proxy; _ } =
    get_ok (settings_from_env ~https_proxy:"   " ())
  in
  check_uri "HTTPS_PROXY" None https_proxy

let test_valid_urls_are_kept_as_is () =
  let { Proxy.http_proxy; https_proxy; all_proxy; _ } =
    get_ok
      (settings_from_env ~http_proxy:"http://proxy.example.com:8080"
         ~https_proxy:"https://proxy.example.com:8443"
         ~all_proxy:"http://proxy.example.com:3128" ())
  in
  check_uri "HTTP_PROXY" (Some "http://proxy.example.com:8080") http_proxy;
  check_uri "HTTPS_PROXY" (Some "https://proxy.example.com:8443") https_proxy;
  check_uri "ALL_PROXY" (Some "http://proxy.example.com:3128") all_proxy

(* Regression test: the scheme-augmenting branch used to log the augmented URI
 * but return the original one, so the host was still missing. *)
let test_missing_scheme_is_augmented () =
  let { Proxy.http_proxy; https_proxy; all_proxy; _ } =
    get_ok
      (settings_from_env ~http_proxy:"proxy.example.com:8080"
         ~https_proxy:"proxy.example.com:8443"
         ~all_proxy:"proxy.example.com:3128" ())
  in
  check_uri "HTTP_PROXY" (Some "http://proxy.example.com:8080") http_proxy;
  check_uri "HTTPS_PROXY" (Some "https://proxy.example.com:8443") https_proxy;
  (* like cohttp, a scheme-less ALL_PROXY defaults to http *)
  check_uri "ALL_PROXY" (Some "http://proxy.example.com:3128") all_proxy

(* An https proxy reached over http is a legitimate setup; don't rewrite
 * the scheme of a URI that already has a host. *)
let test_existing_scheme_is_not_rewritten () =
  let { Proxy.https_proxy; _ } =
    get_ok (settings_from_env ~https_proxy:"http://proxy.example.com:8443" ())
  in
  check_uri "HTTPS_PROXY" (Some "http://proxy.example.com:8443") https_proxy

let test_surrounding_whitespace_is_trimmed () =
  let { Proxy.http_proxy; _ } =
    get_ok (settings_from_env ~http_proxy:" http://proxy.example.com:8080 " ())
  in
  check_uri "HTTP_PROXY" (Some "http://proxy.example.com:8080") http_proxy

let test_hostless_url_is_rejected () =
  let msg = get_error (settings_from_env ~https_proxy:"http://" ()) in
  Alcotest.(check bool)
    "error message names the variable" true
    (String_.contains ~term:"HTTPS_PROXY" msg)

let test_garbage_value_is_rejected () =
  let msg = get_error (settings_from_env ~http_proxy:"not a url" ()) in
  Alcotest.(check bool)
    "error message names the variable" true
    (String_.contains ~term:"HTTP_PROXY" msg)

let test_invalid_all_proxy_is_rejected () =
  let msg = get_error (settings_from_env ~all_proxy:"http://" ()) in
  Alcotest.(check bool)
    "error message names the variable" true
    (String_.contains ~term:"ALL_PROXY" msg)

(* A single-slash typo has no "://", so it used to be treated as scheme-less
 * and augmented into "http://http:/proxy.example.com", whose host parses as
 * "http" with the real host left in the path. That configured cohttp against
 * the wrong host and failed later at DNS time. *)
let test_malformed_scheme_is_rejected () =
  let msg =
    get_error (settings_from_env ~http_proxy:"http:/proxy.example" ())
  in
  Alcotest.(check bool)
    "error message names the variable" true
    (String_.contains ~term:"HTTP_PROXY" msg)

(* Same failure with both slashes missing. *)
let test_scheme_without_slashes_is_rejected () =
  let msg = get_error (settings_from_env ~http_proxy:"http:proxy.example" ()) in
  Alcotest.(check bool)
    "error message names the variable" true
    (String_.contains ~term:"HTTP_PROXY" msg)

(* A proxy is a host and a port; cohttp never reads the path. *)
let test_proxy_url_with_path_is_rejected () =
  let msg =
    get_error (settings_from_env ~http_proxy:"http://proxy.example/path" ())
  in
  Alcotest.(check bool)
    "error message names the variable" true
    (String_.contains ~term:"HTTP_PROXY" msg)

(* A trailing slash is not a path, and must keep working. *)
let test_trailing_slash_is_accepted () =
  let { Proxy.http_proxy; _ } =
    get_ok (settings_from_env ~http_proxy:"http://proxy.example.com:8080/" ())
  in
  check_uri "HTTP_PROXY" (Some "http://proxy.example.com:8080/") http_proxy

(* Proxy URLs often embed credentials; any related error message should
 * redact them, to avoid leakage. *)
let test_error_message_omits_credentials () =
  let msg =
    get_error (settings_from_env ~http_proxy:"http://someone:hunter2@" ())
  in
  Alcotest.(check bool)
    "error message omits the password" false
    (String_.contains ~term:"hunter2" msg)

(* This should still be true even if the variable is malformed, e.g.
 * "user:secret@http://host". It reaches the error path, and the
 * userinfo should still be redacted. *)
let test_error_message_omits_credentials_before_scheme () =
  let msg =
    get_error
      (settings_from_env ~http_proxy:"user:hunter2@http://proxy.example" ())
  in
  Alcotest.(check bool)
    "error message omits the password" false
    (String_.contains ~term:"hunter2" msg)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests =
  Testo.categorize "Proxy"
    [
      t "empty values are ignored" test_empty_values_are_ignored;
      t "whitespace-only values are ignored"
        test_whitespace_only_values_are_ignored;
      t "valid urls are kept as is" test_valid_urls_are_kept_as_is;
      t "missing scheme is augmented" test_missing_scheme_is_augmented;
      t "existing scheme is not rewritten" test_existing_scheme_is_not_rewritten;
      t "surrounding whitespace is trimmed"
        test_surrounding_whitespace_is_trimmed;
      t "hostless url is rejected" test_hostless_url_is_rejected;
      t "garbage value is rejected" test_garbage_value_is_rejected;
      t "invalid ALL_PROXY is rejected" test_invalid_all_proxy_is_rejected;
      t "malformed scheme is rejected" test_malformed_scheme_is_rejected;
      t "scheme without slashes is rejected"
        test_scheme_without_slashes_is_rejected;
      t "proxy url with path is rejected" test_proxy_url_with_path_is_rejected;
      t "trailing slash is accepted" test_trailing_slash_is_accepted;
      t "error message omits credentials" test_error_message_omits_credentials;
      t "error message omits credentials placed before the scheme"
        test_error_message_omits_credentials_before_scheme;
    ]
