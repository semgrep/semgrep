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
(** Username and password pair for proxy authentication *)

type settings = {
  http_proxy : Uri.t option;
      (** HTTP proxy URL. Similar to curl's HTTP_PROXY environment variable.
      Example: "http://proxy.example.com:8080" *)
  https_proxy : Uri.t option;
      (** HTTPS proxy URL. Similar to curl's HTTPS_PROXY environment variable.
      Example: "http://proxy.example.com:8443" *)
  all_proxy : Uri.t option;
      (** Default proxy URL for all protocols if protocol-specific proxy is not set.
      Similar to curl's ALL_PROXY environment variable. *)
  no_proxy : string option;
      (** Comma-separated list of hostnames or domains that should bypass the proxy.
      Similar to curl's NO_PROXY environment variable.
      Example: "localhost,127.0.0.1,.example.com" *)
  credentials : credentials option;
      (** Optional credentials for proxy authentication *)
}
[@@deriving show, eq]
(** Configuration settings for HTTP/HTTPS proxy setup *)

val settings_from_env : unit -> settings
(** Creates proxy settings by reading from environment variables.
    Reads from both lowercase and uppercase variants of:
    HTTP_PROXY, HTTPS_PROXY, ALL_PROXY, NO_PROXY, PROXY_USER, PROXY_PASSWORD *)

val configure_proxy : settings -> unit
(** Configures the LWT HTTP client to use the specified proxy settings *)

val configure_proxy_eio : settings -> unit
(** Configures the EIO HTTP client to use the specified proxy settings *)
