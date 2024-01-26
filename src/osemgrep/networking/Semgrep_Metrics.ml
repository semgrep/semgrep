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
module Http_helpers = Http_helpers.Make (Lwt_platform)

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
  Logs.debug (fun m -> m "Metrics: %s" metrics);
  Logs.debug (fun m -> m "userAgent: '%s'" user_agent);
  let%lwt response =
    Http_helpers.post_async ~body:metrics ~headers caps#network url
  in
  (match response with
  | Ok body -> Logs.debug (fun m -> m "Metrics Endpoint response: %s" body)
  | Error (status_code, err) ->
      Logs.warn (fun m -> m "Metrics Endpoint error: %d %s" status_code err));
  Lwt.return_unit

let send caps = Lwt_platform.run (send_async caps)
