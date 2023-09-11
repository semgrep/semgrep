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

let save_token ?(ident = None) token =
  let settings = Semgrep_settings.load () in
  match Semgrep_App.get_deployment_from_token token with
  | None -> Error "Login token is not valid. Please try again."
  | Some (_name, id)
    when Semgrep_settings.save
           Semgrep_settings.
             {
               settings with
               api_token = Some token;
               anonymous_user_id =
                 Uuidm.v5 Uuidm.nil
                   (match ident with
                   | Some ident -> ident
                   | None -> id);
             } ->
      Ok ()
  | _ -> Error "Failed to save token. Please try again."

let is_logged_in () =
  let settings = Semgrep_settings.load () in
  Option.is_some settings.api_token

let fetch_token ?(min_wait_ms = 2000) ?(next_wait_ms = 1000) ?(max_retries = 12)
    ?(wait_hook = fun _delay_ms -> ()) login_session =
  let apply_backoff current_wait_ms =
    Float.to_int (Float.ceil (Float.of_int current_wait_ms *. 1.3))
  in
  let url =
    Uri.with_path !Semgrep_envvars.v.semgrep_url "api/agent/tokens/requests"
  in
  let body =
    {|{"token_request_key": "|} ^ Uuidm.to_string (fst login_session) ^ {|"}|}
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
        Error msg
    | n -> (
        match Http_helpers.post ~body url with
        | Ok body -> (
            try
              let json = Yojson.Basic.from_string body in
              let open Yojson.Basic.Util in
              match json |> member "token" with
              | `String token ->
                  (* NOTE: We should probably use user_id over user_name for uniqueness constraints *)
                  let ident = json |> member "user_name" |> to_string in
                  Result.bind (save_token ~ident:(Some ident) token) (fun () ->
                      Ok (token, ident))
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
                Error msg))
  in
  fetch_token' next_wait_ms max_retries
