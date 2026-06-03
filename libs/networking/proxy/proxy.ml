(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
type credentials = { username : string; password : string }
[@@deriving show, eq]

type settings = {
  http_proxy : Uri.t option;
  https_proxy : Uri.t option;
  all_proxy : Uri.t option;
  no_proxy : string option;
  (* If we pass in proxy env vars like HTTP_PROXY="http://localhost:8000" PROXY_USER='sal' PROXY_PASSWORD='123',
     they end up being logged. Using [@opaque] here avoids us logging proxy credentials. *)
  credentials : credentials option; [@opaque]
}
[@@deriving show, eq]

(** Environment variable for HTTP proxy URL. *)
let env_http_proxy = "HTTP_PROXY"

(** Environment variable for HTTPS proxy URL. *)
let env_https_proxy = "HTTPS_PROXY"

(** Environment variable for default proxy URL for all protocols. *)
let env_all_proxy = "ALL_PROXY"

(** Environment variable for proxy bypass patterns. Comma-separated list of
    hostnames or domains that should bypass the proxy. *)
let env_no_proxy = "NO_PROXY"

(** Environment variable for proxy authentication username *)
let env_proxy_user = "PROXY_USER"

(** Environment variable for proxy authentication password *)
let env_proxy_password = "PROXY_PASSWORD"

(* for curl compatibility: *)
let getenv k =
  match Sys.getenv_opt (String.lowercase_ascii k) with
  | None -> Sys.getenv_opt (String.uppercase_ascii k)
  | v -> v

(* In the instance where the user supplied e.g HTTP_PROXY=abc.xyz.
 * Cohttp requires that the supplied URI has a scheme. *)
let uri_of_env_with_scheme scheme var =
  let open Common in
  let* uri_string = getenv var in
  let uri = Uri.of_string uri_string in
  (* NOTE that [Uri.scheme domain.com:port] evaluates to [Some domain.com];
   * checking for a host lets us validate if there was a scheme and a host.
   *)
  match Uri.host uri with
  | Some _ ->
      (* NOTE we'd still want to accept [HTTP_PROXY=https://...]*)
      Some uri
  | None ->
      let new_uri_string = Printf.sprintf "%s://%s" scheme uri_string in
      (* nosemgrep: no-logs-in-library *)
      Logs.warn (fun m ->
          m "%s was supplied a URI with no scheme; augmenting it as %s" var
            new_uri_string);
      Some (Uri.of_string uri_string)

let uri_of_env var = Option.map Uri.of_string (getenv var)

let settings_from_env () =
  {
    http_proxy = uri_of_env_with_scheme "http" env_http_proxy;
    https_proxy = uri_of_env_with_scheme "https" env_https_proxy;
    all_proxy = uri_of_env env_all_proxy;
    no_proxy = getenv env_no_proxy;
    credentials =
      (match (getenv env_proxy_user, getenv env_proxy_password) with
      | Some username, Some password -> Some { username; password }
      | Some user, None ->
          (* nosemgrep: no-logs-in-library *)
          Logs.warn (fun m ->
              m
                "%s was set (%s), but %s was not set. Both must be set to be \
                 used."
                env_proxy_user user env_proxy_password);
          None
      | None, Some password ->
          (* nosemgrep: no-logs-in-library *)
          Logs.warn (fun m ->
              m
                "%s was set (%s), but %s was not set. Both must be set to be \
                 used."
                env_proxy_password password env_proxy_user);
          None
      | _ -> None);
  }

let make_scheme_proxy settings =
  let http =
    match settings.http_proxy with
    | Some proxy -> [ ("http", proxy) ]
    | None -> []
  in
  let https =
    match settings.https_proxy with
    | Some proxy -> [ ("https", proxy) ]
    | None -> []
  in
  http @ https

let make_proxy_headers settings =
  match settings.credentials with
  | Some creds ->
      Some
        (Http.Header.init_with "Proxy-Authorization"
        @@ Cohttp.Auth.string_of_credential
             (`Basic (creds.username, creds.password)))
  | _ -> None

let configure_proxy settings =
  let scheme_proxy = make_scheme_proxy settings in
  let all_proxy = settings.all_proxy in
  let proxy_headers = make_proxy_headers settings in
  (* nosemgrep: no-logs-in-library *)
  Logs.info (fun m -> m "Proxy was configured with %a" pp_settings settings);
  Cohttp_lwt_unix.Client.set_cache
    (Cohttp_lwt_unix.Connection_proxy.call
       (Cohttp_lwt_unix.Connection_proxy.create ?all_proxy ~scheme_proxy
          ?no_proxy:settings.no_proxy ?proxy_headers ()))

(* TODO Consider merging with configure_proxy? Or just drop LWT entirely at some
 * point? *)
let configure_proxy_eio settings =
  let scheme_proxies = make_scheme_proxy settings in
  let proxy_headers = make_proxy_headers settings in
  (* nosemgrep: no-logs-in-library *)
  Logs.info (fun m -> m "EIO proxy was configured with %a" pp_settings settings);
  Cohttp_eio.Client.set_proxies ?no_proxy_patterns:settings.no_proxy
    ?default_proxy:settings.all_proxy ~scheme_proxies ?proxy_headers ()
