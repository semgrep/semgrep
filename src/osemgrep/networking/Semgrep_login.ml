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
module Http_helpers = Http_helpers.Make (Lwt_platform)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type login_session = Uuidm.t * Uri.t

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let support_url = "https://semgrep.dev/docs/support/"

let make_login_url () =
  let session_id = Uuidm.v `V4 in
  ( session_id,
    Uri.(
      add_query_params'
        (with_path !Semgrep_envvars.v.semgrep_url "login")
        [
          ("cli-token", Uuidm.to_string session_id);
          ("docker", if !Semgrep_envvars.v.in_docker then "True" else "False");
          ("gha", if !Semgrep_envvars.v.in_gh_action then "True" else "False");
        ]) )

let save_token_async ?(ident = None) token =
  Option.iter
    (fun v -> Logs.debug (fun m -> m "saving token for user %s" v))
    ident;
  let settings = Semgrep_settings.load () in
  Semgrep_App.get_deployment_from_token_async token
  |> Lwt.map (function
       | None -> Error "Login token is not valid. Please try again."
       | Some _deployment_config
         when Semgrep_settings.save
                Semgrep_settings.{ settings with api_token = Some token } ->
           Ok ()
       | _ -> Error "Failed to save token. Please try again.")

let save_token ?(ident = None) token =
  Lwt_platform.run (save_token_async ~ident token)

let is_logged_in () =
  let settings = Semgrep_settings.load () in
  Option.is_some settings.api_token

let default_wait_hook delay_ms =
  (* Note: sleep is measured in seconds *)
  Unix.sleepf (Float.of_int delay_ms /. Float.of_int (1000 * 100))

let fetch_token_async ?(min_wait_ms = 2000) ?(next_wait_ms = 1000)
    ?(max_retries = 12) ?(wait_hook = default_wait_hook) login_session =
  let apply_backoff current_wait_ms =
    Float.to_int (Float.ceil (Float.of_int current_wait_ms *. 1.3))
  in
  let url =
    Uri.with_path !Semgrep_envvars.v.semgrep_url "api/agent/tokens/requests"
  in
  let body =
    {|{"token_request_key": "|} ^ Uuidm.to_string (fst login_session) ^ {|"}|}
  in
  let settings = Semgrep_settings.load () in
  let anonymous_user_id = settings.Semgrep_settings.anonymous_user_id in
  let user_agent = Metrics_.string_of_user_agent () in
  let headers =
    [
      ("Content-Type", "application/json");
      (* include the user_agent which encodes the current semgrep version *)
      ("User-Agent", user_agent);
      (* include the anonymous user id to help with debugging and analysis.*)
      ("X-Semgrep-Client-Id", Uuidm.to_string anonymous_user_id);
    ]
  in
  let rec fetch_token' next_wait_ms' = function
    | 0 ->
        let msg =
          Ocolor_format.asprintf
            "%s Login Failed!\n\
             Your login attempt either timed out or we couldn't connect to \
             Semgrep servers. Please check your internet connection and try \
             again. If this issue persists, please reach out to Semgrep \
             support at @{<cyan;ul>%s@}"
            (Logs_helpers.err_tag ()) support_url
        in
        Lwt.return (Error msg)
    | n -> (
        let%lwt resp = Http_helpers.post_async ~body ~headers url in
        match resp with
        | Ok body -> (
            try
              let json = Yojson.Basic.from_string body in
              let open Yojson.Basic.Util in
              match json |> member "token" with
              | `String token ->
                  (* NOTE: We should probably use user_id over user_name for uniqueness constraints *)
                  let ident = json |> member "user_name" |> to_string in
                  let%lwt result = save_token_async ~ident:(Some ident) token in
                  Result.bind result (fun () -> Ok (token, ident)) |> Lwt.return
              | `Null
              | _ ->
                  let message = Printf.sprintf "Failed to get token: %s" body in
                  Error message |> Lwt.return
            with
            | Yojson.Json_error msg ->
                let message = Printf.sprintf "Failed to parse json: %s" msg in
                Error message |> Lwt.return)
        | Error (status_code, err) -> (
            match status_code with
            | 404 ->
                wait_hook (min_wait_ms + next_wait_ms');
                fetch_token' (apply_backoff next_wait_ms') (n - 1)
            | _ ->
                let msg =
                  Ocolor_format.asprintf
                    "%s Login Failed!\n\
                     We hit an unexpected failure with our endpoint %s (status \
                     code %d).\n\
                     Please try again or reach out to Semgrep support at \
                     @{<cyan;ul>%s@}"
                    (Logs_helpers.err_tag ()) (Uri.to_string url) status_code
                    support_url
                in
                Logs.info (fun m -> m "HTTP error: %s" err);
                Error msg |> Lwt.return))
  in
  fetch_token' next_wait_ms max_retries

let fetch_token ?(min_wait_ms = 2000) ?(next_wait_ms = 1000) ?(max_retries = 12)
    ?(wait_hook = fun _delay_ms -> ()) login_session =
  Lwt_platform.run
    (fetch_token_async ~min_wait_ms ~next_wait_ms ~max_retries ~wait_hook
       login_session)
