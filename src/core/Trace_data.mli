(*
   Copyright (c) 2024-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(** Helpers to prepare data for Opentelemetry tracing *)

(* Types *)

type analysis_flags = {
  secrets_validators : bool;  (** True when secrets validators are enabled *)
  allow_all_origins : bool;
      (** True when secrets validators from any origin may be used. This value
          is discarded if secrets_validators is false *)
  historical_scan : bool;  (** True when historical scans are enabled *)
  deep_intra_file : bool;
      (** True when deep intrafile scans (aka interproc taint) is enabled *)
  deep_inter_file : bool;
      (** True when interfile scans are enabled. Only one of `deep_inter_file`
         and `deep_intra_file` should be true. *)
}
[@@derving show]

(* constants *)
module Attributes : sig
  val jobs : string
  val job : string
  val pro_secrets_validators : string
  val pro_historical_scanning : string
  val pro_deep_intrafile : string
  val pro_deep_interfile : string
  val pro_secrets_allowed_origins : string
end

(* Helpers *)

val no_analysis_features : unit -> analysis_flags
(** For analysis run with the oss engine, we know all the flags will be false *)

val data_of_languages : Analyzer.t list -> (string * Telemetry.user_data) list
(** Convenience function to turn a list of interfile languages into otel data *)

val record_phase_data :
  ?timeout:float ->
  ?memory_limit:int ->
  ?jobs:int ->
  fpaths:Fpath.t list ->
  rules:'a list ->
  Telemetry.scope ->
  unit
(** [record_phase_data ~fpaths ~rules span] records "phase" attributes on a
    span. A phase is any function that takes a set of targets and a set of rules
    as input. This let's us autogenerate and alert on performance metrics that
    are normalized by the size of the input and the number of rules, which are
    two major factors in how long a scan takes. We can also alert on things like
    if we see a lot of scans that have a small number of rules but still take a
    long time, which might be an indication of a perf issue somewhere silly.
*)

val get_resource_attrs :
  ?env:string ->
  engine:string ->
  analysis_flags:analysis_flags ->
  jobs:int ->
  eio:bool ->
  unit ->
  (string * Telemetry.user_data) list
(** [get_resource_data ~engine:"oss" ~env:"prod" ~analysis_flags () ] creates
    tags for the resource we report traces to. This is essentially info about
    the "service" itself, that is immutable once the service/program starts.
    This data is usually useful for grouping large sets of
    logs/traces/errors/metrics and discovering or investigating other macro
    trends about Semgrep. Example: Service Version, OCaml runtime version,
    telemetry sdk version. See module commentary for more info

    Other data besides what's passed in as flags to this function may be
    gathered from the environment such as Semgrep's version number.

    [engine] is the engine we are using, e.g. "oss" or "pro"

    [env] is the environment we are working in ("prod","dev2" etc.). Defaults to
    "prod"

    [analysis_flags] see {!analysis_flags}
  *)
