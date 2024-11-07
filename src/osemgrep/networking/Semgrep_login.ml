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
(* Types *)
(*****************************************************************************)

type shared_secret = Uuidm.t
type login_session = shared_secret * Uri.t

(*****************************************************************************)
(* Weak logged in check *)
(*****************************************************************************)

(* LATER: this does not really check the user is logged in; it just checks
 * whether a token is defined in ~/.semgrep/settings.yml.
 * In theory, we should actually authenticate this token and communicate with
 * the backend to double check (which could slow down the program startup).
 * In fact, some of us generated fake tokens in order to be able to
 * use --pro, especially in CI jobs that was suddenly breaking.
 * A bit like Microsoft back in the days, it is maybe better to not put too
 * strong "piracy" verification and allow users to cheat.
 * Note that Semgrep_settings can also get the token from the environment.
 * coupling: auth.is_logged_in_weak() in pysemgrep
 * TODO: should take Cap.network at least, so when we're ready to move
 * to an actual network (possibly cached) call, we are ready.
 *)
let is_logged_in_weak () =
  let settings = Semgrep_settings.load () in
  Option.is_some settings.api_token

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let support_url = "https://semgrep.dev/docs/support/"

let make_login_url () =
  let rand = Stdlib.Random.State.make_self_init () in
  let session_id = Uuidm.v4_gen rand () in
  ( session_id,
    Uri.(
      add_query_params'
        (with_path !Semgrep_envvars.v.semgrep_url "login")
        [
          ("cli-token", Uuidm.to_string session_id);
          ("docker", if !Semgrep_envvars.v.in_docker then "True" else "False");
          ("gha", if !Semgrep_envvars.v.in_gh_action then "True" else "False");
        ]) )

let save_token_async ?ident caps =
  Option.iter
    (fun v -> Logs.debug (fun m -> m "saving token for user %s" v))
    ident;
  let settings = Semgrep_settings.load () in
  Semgrep_App.get_deployment_from_token_async caps
  |> Lwt.map (function
       | None -> Error "Login token is not valid. Please try again."
       | Some deployment_config
         when Semgrep_settings.save
                Semgrep_settings.{ settings with api_token = Some caps#token }
         ->
           Ok deployment_config
       | _ -> Error "Failed to save token. Please try again.")

let save_token ?ident caps = Lwt_platform.run (save_token_async ?ident caps)

let verify_token_async token =
  let%lwt resopt = Semgrep_App.get_deployment_from_token_async token in
  Lwt.return (Option.is_some resopt)

let verify_token token = Lwt_platform.run (verify_token_async token)

let fetch_token_async ?(min_wait_ms = 2000) ?(next_wait_ms = 1000)
    ?(max_retries = 12) ?(wait_hook = fun _delay_ms -> Lwt.return_unit) caps
    shared_secret =
  let apply_backoff current_wait_ms =
    Float.to_int (Float.ceil (Float.of_int current_wait_ms *. 1.3))
  in
  let url =
    Uri.with_path !Semgrep_envvars.v.semgrep_url "api/agent/tokens/requests"
  in
  let body =
    {|{"token_request_key": "|} ^ Uuidm.to_string shared_secret ^ {|"}|}
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
            (Console.error_tag ()) support_url
        in
        Lwt.return (Error msg)
    | n -> (
        let%lwt resp = Http_helpers.post ~body ~headers caps#network url in
        match resp with
        | Ok { body = Ok body; _ } -> (
            try
              let json = Yojson.Basic.from_string body in
              let open Yojson.Basic.Util in
              match json |> member "token" with
              | `String str_token ->
                  (* NOTE: We should probably use user_id over user_name for uniqueness constraints *)
                  let ident = json |> member "user_name" |> to_string in
                  let token = Auth.unsafe_token_of_string str_token in
                  let caps = Auth.cap_token_and_network token caps in
                  let%lwt result = save_token_async ~ident caps in
                  Result.bind result (fun _deployment_config ->
                      Ok (token, ident))
                  |> Lwt.return
              | `Null
              | _ ->
                  let message = Printf.sprintf "Failed to get token: %s" body in
                  Lwt.return_error message
            with
            | Yojson.Json_error msg ->
                let message = Printf.sprintf "Failed to parse json: %s" msg in
                Lwt.return_error message)
        | Ok { body = Error err; code; _ } -> (
            match code with
            | 404 ->
                let%lwt () = wait_hook (min_wait_ms + next_wait_ms') in
                fetch_token' (apply_backoff next_wait_ms') (n - 1)
            | _ ->
                let msg =
                  Ocolor_format.asprintf
                    "%s Login Failed!\n\
                     We hit an unexpected failure with our endpoint %s (status \
                     code %d).\n\
                     Please try again or reach out to Semgrep support at \
                     @{<cyan;ul>%s@}"
                    (Console.error_tag ()) (Uri.to_string url) code support_url
                in
                Logs.err (fun m -> m "HTTP error: %s" err);
                Lwt.return_error msg)
        | Error e ->
            let msg =
              Ocolor_format.asprintf
                "%s Login Failed!\n\
                 We had an unexpected failure while trying to make a network\n\
                \                  request to %s:\n\
                 %s\n\
                 Please check network settings and try again or reach out to \
                 Semgrep support at @{<cyan;ul>%s@}"
                (Console.error_tag ()) (Uri.to_string url) e support_url
            in
            Logs.err (fun m -> m "Failed to fetch token: %s" e);
            Lwt.return_error msg)
  in

  fetch_token' next_wait_ms max_retries

let fetch_token ?(min_wait_ms = 2000) ?(next_wait_ms = 1000) ?(max_retries = 12)
    ?(wait_hook = fun _delay_ms -> ()) caps shared_secret =
  Lwt_platform.run
    (fetch_token_async ~min_wait_ms ~next_wait_ms ~max_retries
       ~wait_hook:(fun delay_ms ->
         wait_hook delay_ms;
         Lwt.return_unit)
       caps shared_secret)
