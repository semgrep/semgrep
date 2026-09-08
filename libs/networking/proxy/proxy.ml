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

(* A proxy variable set to the empty string in CI commonly indicates
 * "no proxy" (e.g. HTTPS_PROXY="" coming from an unexpanded template
 * variable). Like curl, we treat such a variable as if it had not
 * been set at all: the requests it would have routed through a proxy
 * are sent directly instead. *)
let getenv_proxy_uri_string var =
  match getenv var with
  | Some value when String.trim value = "" ->
      (* nosemgrep: no-logs-in-library *)
      Logs.warn (fun m ->
          m
            "%s is set to an empty value; ignoring it and sending the requests \
             it would have proxied directly instead"
            var);
      None
  | Some value -> Some (String.trim value)
  | None -> None

let scheme_separator = "://"

(* Proxy URLs may embed credentials (e.g. http://user:password@host), which
 * must never end up in a log message or in an error message. We redact the
 * userinfo on the raw string rather than through [Uri.userinfo] so that
 * redaction does not depend on the value being properly formed.
 *
 * Everything before the last '@' is dropped, scheme included: a malformed
 * value can put the credentials first, as in "user:secret@http://host"
 * (composing a proxy URL from CI variables whose base URL already carries a
 * scheme), so there is no prefix we can assume is not a secret. Callers must
 * not rely on the scheme surviving. *)
let redact_userinfo value =
  match String.rindex_opt value '@' with
  | None -> value
  (* [String.rindex_opt] returns a position within the string, so
   * [Str.string_after] keeps at least the '@' itself. *)
  | Some at -> "<redacted>" ^ Str.string_after value at

let has_whitespace = String.exists Base.Char.is_whitespace

(* Cohttp needs a host it can actually resolve. Note that this is stricter
 * than cohttp's own check (which only requires [Uri.host <> None]), but a
 * host that is empty or still carries URI punctuation could never be
 * resolved anyway. *)
let is_usable_proxy_uri uri =
  match Uri.host uri with
  | None -> false
  | Some host -> (
      host <> ""
      (* Not redundant with the check on the raw value: [Uri.host] percent-
       * decodes, so "http://foo%20bar" reaches us here as "foo bar". *)
      && (not (has_whitespace host))
      && (not (String.exists (fun c -> c = '/' || c = '@') host))
      (* A proxy is a host and a port with nothing after it; cohttp only ever
       * resolves the host and port and never reads the path. Requiring an
       * empty path is also what catches a malformed scheme: a value like
       * "http:/proxy.example.com" contains no "://", so it gets augmented to
       * "http://http:/proxy.example.com", out of which [Uri] parses the host
       * "http" and leaves the real host in the path. *)
      &&
      match Uri.path uri with
      | ""
      | "/" ->
          true
      | _ -> false)

(* In the instance where the user supplied e.g HTTP_PROXY=abc.xyz.
 * Cohttp requires that the supplied URI has a scheme.
 *
 * Returns [Error msg] for a non-empty value that we cannot turn into a URI
 * with a host: cohttp raises [Invalid_argument] on such a URI, either when
 * setting up the connection cache or when performing the first request, so
 * returning that URI here would crash the scan. *)
let uri_of_env_with_scheme scheme var =
  match getenv_proxy_uri_string var with
  | None -> Ok None
  | Some value ->
      let invalid () =
        Error
          (`Msg
             (Printf.sprintf
                "%s is set to %s, which is not a valid proxy URL. Expected \
                 something like http://proxy.example.com:8080. Unset %s to run \
                 without a proxy."
                var (redact_userinfo value) var))
      in
      (* No proxy URL contains whitespace, and [Uri.of_string] would otherwise
       * silently parse a host out of the first word of e.g. "not a url". *)
      if has_whitespace value then invalid ()
      else
        (* We test for "://" rather than consulting [Uri.scheme] because
         * [Uri.scheme "domain.com:port"] evaluates to [Some "domain.com"],
         * which would make a scheme-less value look like it had one.
         *
         * Only a value with no scheme gets one added: augmenting a value that
         * already has a scheme but no host (e.g. "http://") would build a
         * nonsense URI such as "https://http://", out of which [Uri] parses a
         * plausible-looking host that could never resolve. *)
        let uri =
          match String_.search ~term:scheme_separator value with
          (* NOTE we'd still want to accept [HTTP_PROXY=https://...] *)
          | Some _ -> Uri.of_string value
          | None ->
              let augmented =
                Printf.sprintf "%s%s%s" scheme scheme_separator value
              in
              (* Report the scheme we assumed rather than the augmented URI:
               * that URI would have to be redacted, and redaction drops the
               * scheme, which would leave a warning claiming an augmentation
               * it does not show. *)
              (* nosemgrep: no-logs-in-library *)
              Logs.warn (fun m ->
                  m "%s was supplied a URI with no scheme; assuming %s" var
                    scheme);
              Uri.of_string augmented
        in
        if is_usable_proxy_uri uri then Ok (Some uri) else invalid ()

(* Both PROXY_USER and PROXY_PASSWORD must be set to authenticate; with only
 * one of them we send no credentials at all. Note that a variable set to the
 * empty string counts as set, so an empty password ("PROXY_PASSWORD=") stays
 * usable, as the HTTP Basic scheme allows it.
 *
 * The messages name the variables but never their values: one of the two is a
 * password, and these warnings end up in CI logs. *)
let credentials_from_env () =
  let both_must_be_set set_var unset_var =
    (* nosemgrep: no-logs-in-library *)
    Logs.warn (fun m ->
        m
          "%s was set, but %s was not. Both must be set to authenticate to a \
           proxy; proceeding without credentials."
          set_var unset_var);
    None
  in
  match (getenv env_proxy_user, getenv env_proxy_password) with
  | Some username, Some password -> Some { username; password }
  | Some _, None -> both_must_be_set env_proxy_user env_proxy_password
  | None, Some _ -> both_must_be_set env_proxy_password env_proxy_user
  | None, None -> None

let settings_from_env () =
  let ( let* ) = Result.bind in
  let* http_proxy = uri_of_env_with_scheme "http" env_http_proxy in
  let* https_proxy = uri_of_env_with_scheme "https" env_https_proxy in
  (* http matches what cohttp's tunnel path does with a scheme-less proxy URI;
   * its direct path adds no scheme at all, so augmenting here is also what
   * makes a scheme-less ALL_PROXY usable rather than rejected. *)
  let* all_proxy = uri_of_env_with_scheme "http" env_all_proxy in
  Ok
    {
      http_proxy;
      https_proxy;
      all_proxy;
      no_proxy = getenv env_no_proxy;
      credentials = credentials_from_env ();
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
