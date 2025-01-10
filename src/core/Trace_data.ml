(* Emma Jin
 *
 * Copyright (C) 2024 Semgrep Inc.
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
(* NOTE: [^0] is a footnote *)
(* Helpers to prepare attributes for Opentelemetry. Attributes[^0] are key-value
   pairs that are used for sorting and filtering telemetry data, and also for
   attaching info that may be relevant to the telemetry data. This module helps
   organize these attributes, and provides some attributes that we always want
   to set.

   In general, there are two places we can add "attributes" to Opentelemetry
   data. There are "resource" attributes, where a resource[^1] is basically a
   service or program (think a rest API, a database, semgrep itself) that is
   emitting telemetry. Resource attributes usually help categorize and describe
   what's emitting telemetry. Examples:
   * Version
   * Deployment environment (develop, staging, production...)
   * Name of the deployment (semgrep, semgrep-app...)
   * Runtime version (OCaml 4.15, OCaml 5.0...)
   * How the resource was executed (# of jobs, cli flags passed, )
   * Commonly defined resource attributes: https://opentelemetry.io/docs/specs/semconv/resource/

   Some of these resourcs are handled in a special way by opentelemetry[^2], or
   by tools that ingest opentelemetry data, like prometheus[^3] and datadog[4].
   That means we have to be careful what we set as resource attributes, as it
   can break alarms/monitors, dashboards, or tools as a whole altogether.
   There's a lot of rules of what attributes are used where, so if you are
   setting an attribute that's handles in a special way, please review the
   linked docs. These attributes are set in the ocaml otel sdk by setting the
   `global attributes` (this is a misnomer and not according to spec, see issue
   here[^5]). In general these attributes should be information that is immutable
   once the program starts.

   The other kind of attributes are for any kind of opentelemetry event (traces,
   logs, metrics), and are used to describe said event, or attach relevant info.
   Examples:
   * Line/file a log was recorded
   * Stacktrace of an error in a trace
   * Status of a trace (success, error)
   * Args of a function being traced
   * Category of metric being recorded (whether a file was scanned succesfully
     or not)

   Like resource attributes, there are commonly defined resource attributes[^5].
   These usually don't have any special handling.

   footnotes:
   [^0] https://opentelemetry.io/docs/specs/otel/common/#attribute
   [^1] https://opentelemetry.io/docs/specs/otel/resource/sdk/
   [^2] https://opentelemetry.io/docs/specs/semconv/resource/#attributes-with-special-handling
   [^3] https://opentelemetry.io/docs/specs/otel/compatibility/prometheus_and_openmetrics/#resource-attributes-1
   [^4] https://docs.datadoghq.com/opentelemetry/schema_semantics/
   [^5] https://opentelemetry.io/docs/specs/semconv/
*)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

(* Only add Semgrep specific attributes here, the rest should go in Tracing.ml
   (like ocaml runtime version, if we're in a container etc.) *)
module Attributes = struct
  (* Scan related attrs *)
  let semgrep_managed_scan = "scan.semgrep_managed_scan"
  let engine = "scan.engine"
  let repo_name = "scan.repo_name"
  let jobs = "scan.jobs"
  let job = "scan.parmap_job"
  let folder = "scan.folder"
  let pro_secrets_validators = "scan.pro_secrets_validators"
  let pro_historical_scanning = "scan.pro_historical_scanning"
  let pro_deep_intrafile = "scan.pro_deep_intrafile"
  let pro_deep_interfile = "scan.pro_deep_interfile"
  let pro_secrets_allowed_origins = "scan.pro_secrets_allowed_origins"
end
(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type analysis_flags = {
  secrets_validators : bool;
  allow_all_origins : bool;
  historical_scan : bool;
  deep_intra_file : bool;
  deep_inter_file : bool;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Set the descriptor for allowed origins. This is not simply
   a boolean because we will likely include new origins in the
   future *)
let allowed_origins allow_all_origins =
  if allow_all_origins then "all_origins" else "pro_rules_only"

let get_env_vars =
  (* just get the first env var that is set in a list of env vars *)
  let get_first_env_var env_vars : string option =
    try
      match env_vars |> List_.map Sys.getenv_opt |> List_.filter_some with
      | hd :: _ -> Some hd
      | [] -> None
      (* any Sys.* function can raise Sys_error :( *)
    with
    | Sys_error e ->
        (* We probably want to see this error since it'd be really weird if it
           happened *)
        (* nosemgrep *)
        Logs.err (fun m ->
            m
              "System error reading an environment variable for tracing data: \
               %s"
              e);
        None
  in
  let map_env_var_to_otel_data name type_ env_vars default :
      string * Trace_core.user_data =
    let user_data_val_of_string x =
      let v_opt =
        match type_ with
        | `Int -> int_of_string_opt x |> Option.map (fun x -> `Int x)
        | `String -> Some (`String x)
        | `Bool -> Some (`Bool (x = "true" || x = "1" || x = "yes"))
        | `Float -> float_of_string_opt x |> Option.map (fun x -> `Float x)
        | `None -> None
      in
      Option.value v_opt ~default
    in
    let user_data =
      get_first_env_var env_vars
      |> Option.fold ~none:default ~some:user_data_val_of_string
    in
    (name, user_data)
  in
  List_.map (fun (name, type_, env_vars, default) ->
      map_env_var_to_otel_data name type_ env_vars default)

(* In case we don't have a repo name, report the base folder where
   semgrep was run. We report only the base name to avoid leaking
   user information they may not have expected us to include. *)
let current_working_folder () = Filename.basename (Sys.getcwd ())

(*****************************************************************************)
(* Defaults *)
(*****************************************************************************)
(* Format:
   (name, type, env_vars, default_value)
   where we pick the first env var that is set, and if none are set we use the
   default value
*)

(* Resource attributes we always want to try and set from the environment *)
let default_resource_env_attrs =
  [
    (* coupling: semgrep/meta.py, if you change this we may want to change
       something about job url there, or vice versa *)
    (* Instance of the semgrep `service` *)
    ( Tracing.Attributes.instance_id,
      `String,
      [ "SEMGREP_JOB_URL"; "CI_JOB_URL" ],
      `None );
    (* coupling: semgrep/meta.py, if you change this we may want to change
       something about job url there, or vice versa *)
    ( Attributes.semgrep_managed_scan,
      `Bool,
      [ "SEMGREP_MANAGED_SCAN" ],
      `Bool false );
    (* Poor man's Git repo detection. Running git repo detection again
       seems wasteful, but checking two env vars is pretty cheap.

       TODO the more we port of semgrep scan and semgrep ci, the more
       of this information will already be in OCaml *)
    ( Attributes.repo_name,
      `String,
      [ "SEMGREP_REPO_DISPLAY_NAME"; "SEMGREP_REPO_NAME" ],
      `String "<local run>" );
  ]

(*****************************************************************************)
(* Shortcuts for Otel tracing *)
(*****************************************************************************)

let no_analysis_features () =
  {
    secrets_validators = false;
    historical_scan = false;
    allow_all_origins = false;
    deep_intra_file = false;
    deep_inter_file = false;
  }

let data_of_languages (languages : Analyzer.t list) =
  languages |> List_.map (fun l -> (Analyzer.to_string l, `Bool true))

(* NOTE: If this IS NOT semgrep specific stick it in Tracing.ml *)
(* WARNING: Let's be careful what we add as a resource attribute. TL;DR; these
   are used in different ways by the tools that ingest otel data , and certain
   types of data can have different performance and cost implications for these
   tools. See module commentary for more info
*)
let get_resource_attrs ?(env = "prod") ~engine ~analysis_flags ~jobs () =
  [
    (* Version of Semgrep *)
    (Tracing.Attributes.version, `String Version.version);
    (* Whether we're running in a production, staging, or develop environment
       (Usually maps to SMS prod,staging,dev2) *)
    (Tracing.Attributes.deployment_environment_name, `String env);
    (Attributes.engine, `String engine);
    (Attributes.jobs, `Int jobs);
    (Attributes.folder, `String (current_working_folder ()));
    (Attributes.pro_secrets_validators, `Bool analysis_flags.secrets_validators);
    (Attributes.pro_historical_scanning, `Bool analysis_flags.historical_scan);
    (Attributes.pro_deep_intrafile, `Bool analysis_flags.deep_intra_file);
    (Attributes.pro_deep_interfile, `Bool analysis_flags.deep_inter_file);
    (* TODO it would be nice if we also got how the process was executed, and
       with what config/flags *)
  ]
  @ get_env_vars default_resource_env_attrs
  @
  if analysis_flags.secrets_validators then
    [
      ( Attributes.pro_secrets_allowed_origins,
        `String (allowed_origins analysis_flags.allow_all_origins) );
    ]
  else []
