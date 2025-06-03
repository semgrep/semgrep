type credentials = { username : string; password : string }
[@@deriving show, eq]

type settings = {
  http_proxy : Uri.t option;
  https_proxy : Uri.t option;
  all_proxy : Uri.t option;
  no_proxy : string option;
  credentials : credentials option;
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

let uri_of_env var = Option.map Uri.of_string (getenv var)

let settings_from_env () =
  {
    http_proxy = uri_of_env env_http_proxy;
    https_proxy = uri_of_env env_https_proxy;
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
  (* TODO(eio): This is specific to using Cohttp_lwt for networking. When we
     switch to EIO, this will need to be updated. Since we will probably need a
     separate client for multiple users (instead of a mutable singleton) we'll
     need to change the structure here a bit, but the important thing is when
     creating such clients we ensure we configure the proxy appropriately, as
     we do here. *)
  (* nosemgrep: no-logs-in-library *)
  Logs.info (fun m -> m "Proxy was configured with %a" pp_settings settings);
  Cohttp_lwt_unix.Client.set_cache
    (Cohttp_lwt_unix.Connection_proxy.call
       (Cohttp_lwt_unix.Connection_proxy.create ?all_proxy ~scheme_proxy
          ?no_proxy:settings.no_proxy ?proxy_headers ()))
