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

let save_token token =
  let settings = Semgrep_settings.load () in
  let settings = Semgrep_settings.{ settings with api_token = Some token } in
  match Semgrep_App.get_deployment_from_token token with
  | None -> Error "Login token is not valid. Please try again."
  | Some _ when Semgrep_settings.save settings -> Ok ()
  | _ -> Error "Failed to save token. Please try again."

let is_logged_in () =
  let settings = Semgrep_settings.load () in
  Option.is_some settings.api_token

let fetch_token ?(min_wait = 2000) ?(next_wait = 1000) ?(max_retries = 12)
    ?(wait_hook = fun () -> ()) login_session =
  let apply_backoff current_wait =
    Float.to_int (Float.ceil (Float.of_int current_wait *. 1.3))
  in
  let url =
    Uri.with_path !Semgrep_envvars.v.semgrep_url "api/agent/tokens/requests"
  in
  let body =
    {|{"token_request_key": "|} ^ Uuidm.to_string (fst login_session) ^ {|"}|}
  in
  let rec fetch_token' next_wait' = function
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
        Error msg
    | n -> (
        match Http_helpers.post ~body url with
        | Ok body -> (
            try
              let json = Yojson.Basic.from_string body in
              let open Yojson.Basic.Util in
              match json |> member "token" with
              | `String token ->
                  Result.bind (save_token token) (fun () ->
                      Ok (token, json |> member "user_name" |> to_string))
              | `Null
              | _ ->
                  let message = Printf.sprintf "Failed to get token: %s" body in
                  Error message
            with
            | Yojson.Json_error msg ->
                let message = Printf.sprintf "Failed to parse json: %s" msg in
                Error message)
        | Error (status_code, err) -> (
            match status_code with
            | 404 ->
                wait_hook ();
                (* 100 steps for each iteration with variable sleep time between *)
                Unix.sleepf (Float.of_int (min_wait + next_wait));

                fetch_token' (apply_backoff next_wait') (n - 1)
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
                Error msg))
  in
  fetch_token' next_wait max_retries
