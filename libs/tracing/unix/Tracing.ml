(* Emma Jin
 *
 * Copyright (C) 2023 Emma Jin
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

module Otel = Opentelemetry

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(** Tracing library for Semgrep using several libraries:
 *
 * - trace (https://github.com/c-cube/ocaml-trace) for the trace
     instrumentation frontend (e.g. the annotations)
 * - opentelemetry (https://github.com/imandra-ai/ocaml-opentelemetry)
     for the backend that processes traces
 * - opentelemetry-client-ocurl (included with opentelemetry) for the
     collector. TODO use opentelemetry-client-cohttp-lwt instead since
     we rely on cottp in other places already
 * - ambient-context (https://github.com/ELLIOTTCABLE/ocaml-ambient-context)
     which we set up for opentelemetry to use
 *
 * The goal of tracing is to track how we perform in real scans. Things we
 * might do with this data include tracking the p95 scan time, tracking the
 * p95 scan time of a particular phase, alerting on significantly large scans,
 * or digging into the trace of a scan that's taking too long to figure out
 * where it's taking the most time.
 *
 * We use the `trace` frontend for instrumenting the code so that if we want
 * to use a different backend (permanently, or for our own profiling), we can
 * switch it out in just this file.
 *
 * Functions can be instrumented using a ppx or directly with the `with_span`
 * function. The results are sent to the default endpoint (see constants below),
 * which collects them to send to a viewer.
 *
 * If you want to send traces to a different endpoint, prepend your command with
 * `SEMGREP_OTEL_ENDPOINT=<url>`
 *
 * TODO we'll probably need instructions for some system of tags?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type asdf_span = Int64.t [@@deriving show]

type analysis_flags = {
  secrets_validators : bool;
  allow_all_origins : bool;
  historical_scan : bool;
  deep_intra_file : bool;
  deep_inter_file : bool;
}
[@@deriving show]

type top_level_data = { version : string; analysis_flags : analysis_flags }
[@@deriving show]

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

let default_endpoint = "https://telemetry.dev2.semgrep.dev"
let endpoint_env_var = "SEMGREP_OTEL_ENDPOINT"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let no_analysis_features () =
  {
    secrets_validators = false;
    historical_scan = false;
    allow_all_origins = false;
    deep_intra_file = false;
    deep_inter_file = false;
  }

(* Set the descriptor for allowed origins. This is not simply
   a boolean because we will likely include new origins in the
   future *)
let allowed_origins allow_all_origins =
  if allow_all_origins then "all_origins" else "pro_rules_only"

(* Poor man's Git repo detection. Running git repo detection again
   seems wasteful, but checking two env vars is pretty cheap.

   TODO the more we port of semgrep scan and semgrep ci, the more
   of this information will already be in OCaml *)
let repo_name () =
  match Sys.getenv_opt "SEMGREP_REPO_DISPLAY_NAME" with
  | Some name -> name
  | None -> (
      match Sys.getenv_opt "SEMGREP_REPO_NAME" with
      | Some name -> name
      | None -> "<local run>")

(* In case we don't have a repo name, report the base folder where
   semgrep was run. We report only the base name to avoid leaking
   user information they may not have expected us to include. *)
let current_working_folder () = Filename.basename (Sys.getcwd ())

(*****************************************************************************)
(* Wrapping functions Trace gives us to instrument the code *)
(*****************************************************************************)

let enter_span = Trace_core.enter_span
let exit_span = Trace_core.exit_span
let with_span = Trace_core.with_span
let add_data_to_span = Trace_core.add_data_to_span

let add_data_to_opt_span sp data =
  Option.iter (fun sp -> Trace_core.add_data_to_span sp data) sp

(*****************************************************************************)
(* Entry points for setting up tracing *)
(*****************************************************************************)
(* Set according to README of https://github.com/imandra-ai/ocaml-opentelemetry/ *)
let configure_tracing service_name =
  Otel.Globals.service_name := service_name;
  Otel.GC_metrics.basic_setup ();
  Ambient_context.set_storage_provider (Ambient_context_lwt.storage ());
  let otel_backend = Opentelemetry_client_ocurl.create_backend () in
  (* This forwards the spans from Trace to the Opentelemetry collector *)
  Opentelemetry_trace.setup_with_otel_backend otel_backend

let with_setup fname (config : top_level_data) f =
  (* This sets up the OTel collector and runs the given function.
   * Note that the function is traced by default. This makes sure we
     always trace the given function; it also ensures that all the spans from
     the given run are nested under a single trace.
   * ALT: we could also have wrapped this with a `Otel.Scope.with_ambient_scope`
     to ensure the trace_id is the same for all spans, but we decided that
     having the top level time is a good default. *)
  let url =
    match Sys.getenv_opt endpoint_env_var with
    | Some url -> url
    | None -> default_endpoint
  in
  let data () =
    [
      ("version", `String config.version);
      ("folder", `String (current_working_folder ()));
      ("repo_name", `String (repo_name ()));
      ("pro_secrets_validators", `Bool config.analysis_flags.secrets_validators);
      ("pro_historical_scanning", `Bool config.analysis_flags.historical_scan);
      ("pro_deep_intrafile", `Bool config.analysis_flags.deep_intra_file);
      ("pro_deep_interfile", `Bool config.analysis_flags.deep_inter_file);
    ]
    @
    if config.analysis_flags.secrets_validators then
      [
        ( "pro_secrets_allowed_origins",
          `String (allowed_origins config.analysis_flags.allow_all_origins) );
      ]
    else []
  in
  let config = Opentelemetry_client_ocurl.Config.make ~url () in
  Opentelemetry_client_ocurl.with_setup ~config () @@ fun () ->
  with_span ~__FILE__ ~__LINE__ ~data fname @@ fun sp -> f sp

(* Alt: using cohttp_lwt

   Lwt_platform.run (let res = Opentelemetry_client_cohttp_lwt.with_setup ~config () @@ fun () ->
   run_with_span "All time" f in
     Lwt.bind (Lwt_platform.sleep 0.01) (fun () -> Lwt.return res)) *)
