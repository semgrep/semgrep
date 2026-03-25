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
  let jobs = "scan.core.jobs"
  let job = "scan.parmap_job"
  let pro_secrets_validators = "scan.core.pro_secrets_validators"
  let pro_historical_scanning = "scan.core.pro_historical_scanning"
  let pro_deep_intrafile = "scan.core.pro_deep_intrafile"
  let pro_deep_interfile = "scan.core.pro_deep_interfile"
  let pro_secrets_allowed_origins = "scan.core.pro_secrets_allowed_origins"
  let phase_targets_count = "scan.phase.targets.count"
  let phase_targets_size = "scan.phase.targets.bytes"
  let phase_rules_count = "scan.phase.rules.count"
  let phase_jobs_count = "scan.phase.jobs.count"
  let phase_timeout = "scan.phase.timeout_s"
  let phase_memory_limit = "scan.phase.memory_limit_mb"
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
  languages |> List.map (fun l -> (Analyzer.to_string l, `Bool true))

(*
 record_phase_data records metrics for functions that take both sets of files
 and sets of rules as inputs. This let's us autogenerate and alert on
 performance metrics that are normalized by the size of the input and the
 number of rules, which are two major factors in how long a scan takes. We can
 also alert on things like if we see a lot of scans that have a small number of
 rules but still take a long time, which might be an indication of a perf issue
 somewhere silly.
 *)
(* coupling: telemetry.py add_phase_data *)
let record_phase_data ?(timeout = 0.0) ?(memory_limit = 0) ?(jobs = 1) ~fpaths
    ~rules sp =
  let filesize fpath =
    match UFile.filesize fpath with
    | Ok size -> size
    | _ -> 0
  in
  let attrs : (string * Opentelemetry.value) list =
    [
      (Attributes.phase_targets_count, `Int (List.length fpaths));
      ( Attributes.phase_targets_size,
        `Int (List.fold_left (fun acc path -> acc + filesize path) 0 fpaths) );
      (Attributes.phase_rules_count, `Int (List.length rules));
      (Attributes.phase_jobs_count, `Int jobs);
      (Attributes.phase_memory_limit, `Int memory_limit);
      (Attributes.phase_timeout, `Float timeout);
    ]
  in
  Tracing.add_data_to_span sp attrs

(* NOTE: If this IS NOT semgrep specific stick it in Tracing.ml *)
(* WARNING: Let's be careful what we add as a resource attribute. TL;DR; these
   are used in different ways by the tools that ingest otel data , and certain
   types of data can have different performance and cost implications for these
   tools. See module commentary for more info
*)
let get_resource_attrs ?(env = "prod") ~engine ~analysis_flags ~jobs ~eio () =
  let attrs =
    [
      (* Version of Semgrep *)
      (Telemetry.Attributes.version, `String Version.version);
      (* Whether we're running in a production, staging, or develop environment
       (Usually maps to SMS prod,staging,dev2) *)
      (Telemetry.Attributes.deployment_environment_name, `String env);
      (Telemetry.Attributes.scan_engine, `String engine);
      (Telemetry.Attributes.eio, `Bool eio);
      (Attributes.jobs, `Int jobs);
      ( Attributes.pro_secrets_validators,
        `Bool analysis_flags.secrets_validators );
      (Attributes.pro_historical_scanning, `Bool analysis_flags.historical_scan);
      (Attributes.pro_deep_intrafile, `Bool analysis_flags.deep_intra_file);
      (Attributes.pro_deep_interfile, `Bool analysis_flags.deep_inter_file);
      (* TODO it would be nice if we also got how the process was executed, and
       with what config/flags *)
    ]
    @
    if analysis_flags.secrets_validators then
      [
        ( Attributes.pro_secrets_allowed_origins,
          `String (allowed_origins analysis_flags.allow_all_origins) );
      ]
    else []
  in
  (* Filter out if the val is `None *)
  List.filter (fun (_, v) -> not (v = `None)) attrs
