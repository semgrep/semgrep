(* Yoann Padioleau
 *
 * Copyright (C) 2023 Semgrep Inc.
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
(* Code to interact with the metrics.semgrep.dev endpoint
 *
 * See also semgrep_metrics.atd and Metrics_.ml
 *
 * alt: we could implement send() in core/Metrics_.ml,
 * but we would need to add a dependency on Http_helpers, so cleaner
 * to separate the business logic in Metrics_.ml from web interaction
 * in Semgrep_Metrics.ml.
 *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)
let send_async caps =
  (* Populate the sent_at timestamp *)
  Metrics_.prepare_to_send ();
  let user_agent = Metrics_.string_of_user_agent () in
  let metrics = Metrics_.string_of_metrics () in
  let url = !Semgrep_envvars.v.metrics_url in
  let headers =
    [ ("Content-Type", "application/json"); ("User-Agent", user_agent) ]
  in
  (* TODO: the metrics can be big, maybe skip logging it if too big,
   * especially the fileStats and rule performance stats.
   *)
  Logs.debug (fun m ->
      m "Sending metrics (with user agent '%s') data: %s" user_agent metrics);
  let%lwt response =
    Http_helpers.post ~body:metrics ~headers caps#network url
  in
  (match response with
  | Ok { body = Ok body; _ } -> (
      (* TODO: find where the schema of the response is defined and
       * add it in semgrep_metrics.atd
       * Here is an example of answer:
       *       { "errorType":"TypeError",
       *         "errorMessage":"Cannot read property 'map' of undefined",
       *          "trace":[
       *             "TypeError: Cannot read property 'map' of undefined",
       *             "    at createPerRuleObjects (/var/task/index.js:287:24)",
       *             "    at Runtime.exports.handler (/var/task/index.js:363:20)",
       *           ]
       *        }
       *
       *)
      try
        let json = JSON.json_of_string body in
        match json with
        | Object (("errorType", _) :: _) ->
            Logs.warn (fun m -> m "Metrics server error: %s" body)
        | _else_ -> Logs.debug (fun m -> m "Metrics server response: %s" body)
      with
      | Yojson.Json_error msg ->
          Logs.warn (fun m -> m "Metrics response is not valid json: %s" msg))
  | Ok { body = Error err; code; _ } ->
      Logs.warn (fun m -> m "Metrics server error: %d %s" code err)
  | Error e -> Logs.warn (fun m -> m "Failed to send metrics: %s" e));
  Lwt.return_unit

let send caps =
  Logs.info (fun m -> m "Sending metrics");
  Lwt_platform.run (send_async caps)
